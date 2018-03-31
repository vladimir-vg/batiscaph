-module(vision_probe_protocol).
-behaviour(gen_server).
-export([upgrade/4]). % cowboy callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks
-export([after_terminate/2]). % gen_tracker callback
-export([mention_used_atoms/0]).



-record(persistent, {
  socket,
  transport,
  sent_requests = #{}, % non_neg_integer() => atom()
  last_req_num = 0,

  user_id,
  instance_id,
  test_subscribers = []
}).



upgrade(Req, Env, _Handler, _HandlerOpts) ->
  {Token, Req1} = cowboy_req:header(<<"x-runtime-vision-token">>, Req),
  case authorize_by_token(Token) of
    badtoken ->
      {ok, Req2} = cowboy_req:upgrade_reply(403, [], Req1),
      {ok, Req2, Env};

    {ok, UserId} -> start_persistent_loop(Env, UserId, Req1)
  end.

authorize_by_token(Token) ->
  [UserId, _Salt, _Hash] = binary:split(Token, <<":">>, [global]),
  % TODO: fetch probe_key for given user, and compare hashes
  {ok, binary_to_integer(UserId)}.



start_persistent_loop(Env, UserId, Req) ->
  ok = remove_from_ranch_connection_pool(Env),
  InstanceId = vision_util:binary_to_hex(crypto:strong_rand_bytes(32)),
  {ok, Socket, Transport, _Req1} = switch_socket_to_binary_stream(InstanceId, Req),
  lager:info("Socket ~p was open open on id: ~p", [Socket, InstanceId]),

  % now we turning this process into gen_server
  % add necessary dict values to make supervisor happy
  put('$ancestors', [self()]),
  State = #persistent{socket = Socket, transport = Transport, user_id = UserId, instance_id = InstanceId},
  {ok, State1} = insert_new_instance_into_db(InstanceId, State),
  ok = attach_to_gen_tracker(InstanceId),

  ok = vision_clk_events:insert([
    vision_event:event(InstanceId, now, <<"0 vision connection-start">>)
  ]),

  self() ! {probe_request, get_user_config, []},

  {ok, State2} = check_test_subscribers(State1),
  ok = notify_subscribers_about_connection(State2),
  gen_server:enter_loop(?MODULE, [], State2).



remove_from_ranch_connection_pool(Env) ->
  % take this socket out of pool
  % more details in ranch official documentation
  Ref = proplists:get_value(listener, Env),
  ok = ranch:remove_connection(Ref).

switch_socket_to_binary_stream(_InstanceId, Req) ->
  Headers = [
    {<<"upgrade">>, <<"application/vision-persistent-v0">>}
    % {<<"x-runtime-vision-instance-id">>, InstanceId}
  ],
  {ok, Req1} = cowboy_req:upgrade_reply(101, Headers, Req),
  [Socket, Transport] = cowboy_req:get([socket, transport], Req1),
  Transport:setopts(Socket, [{active,once},{packet,4}]),

  % cowboy sends this message to self to indicate that sent was successful
  % read it out from mailbox
  receive {cowboy_req,resp_sent} -> ok
  after 1000 -> error(failed_cowboy_upgrade_reply)
  end,
  {ok, Socket, Transport, Req1}.

attach_to_gen_tracker(InstanceId) ->
  % provided start_link function don't even exist
  % MFA provided to give gen_tracker an idea where to search for
  % after_terminate callback
  ChildSpec = {InstanceId, {vision_probe_protocol, start_link, []}, temporary, 200, worker, []},
  gen_tracker:add_existing_child(probes, {self(), ChildSpec}),
  ok.

notify_subscribers_about_connection(#persistent{test_subscribers = Pids, instance_id = InstanceId}) ->
  [Pid ! {probe_connected, InstanceId} || Pid <- Pids],
  ok.



%
%
%



init(_) ->
  error(not_supposed_to_be_started_directly).



% gen_tracker callback
% executed when process is already dead,
% but its attrs are not removed yet
after_terminate(InstanceId, _Attrs) ->
  ok = vision_clk_events:insert([
    vision_event:event(InstanceId, now, <<"0 vision connection-stop">>)
  ]),
  ok.



% request by this process itself
handle_info({probe_request, Method, Arg}, State) ->
  {ok, State1} = request_probe(probe_request, Method, Arg, State),
  {noreply, State1};

handle_info({tcp_closed, Socket}, #persistent{socket = Socket} = State) ->
  lager:info("Socket ~p closed", [Socket]),
  {stop, normal, State};

handle_info({tcp, Socket, Data}, #persistent{socket = Socket, transport = Transport} = State) ->
  Transport:setopts(Socket, [{active,once}]),
  {ok, State1} = handle_data_from_probe(Data, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call({request, Method, Arg}, From, #persistent{} = State) ->
  {ok, State1} = request_probe(From, Method, Arg, State),
  {noreply, State1};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_data_from_probe(Data, #persistent{} = State) ->
  Term = erlang:binary_to_term(Data, [safe]),
  % check subsribers just right before receiving/sending for now
  % rework later to receive subscribe requests
  {ok, State1} = check_test_subscribers(State),
  [Pid ! {from_probe, Term} || Pid <- State1#persistent.test_subscribers],
  {ok, State2} = handle_message_from_probe(Term, State1),
  {ok, State2}.



handle_message_from_probe({request, ReqId, Method, Arg}, State) ->
  {ok, Result, State1} = handle_request(Method, Arg, State),
  lager:info("probe request ~p ~p ~p -> ~p", [ReqId, Method, Arg, Result]),
  {ok, State2} = send_to_probe({response, ReqId, Method, Result}, State1),
  {ok, State2};

handle_message_from_probe({response, ReqId, Method, Result}, State) ->
  {ok, State1} = response_from_probe(ReqId, Method, Result, State),
  {ok, State1};

handle_message_from_probe({summary_info, Info}, State) ->
  #{dependency_in := _, probe_version := _} = Info,
  {ok, State};

handle_message_from_probe({events, Events}, #persistent{instance_id = InstanceId} = State) ->
  % [lager:info("event: ~p", [E]) || E <- Events],
  Events1 = vision_event:transform(Events, #{instance_id => InstanceId}),
  ok = vision_clk_events:insert(Events1),
  {ok, State};

handle_message_from_probe(Message, State) ->
  lager:info("got unknown message from probe: ~p", [Message]),
  {ok, State}.



%
%
%



insert_new_instance_into_db(InstanceId, #persistent{user_id = UserId} = State) ->
  vision_db:query(insert_new_instance, fun (C, SQL) ->
    {ok, 1} = epgpool:equery(C, SQL, [InstanceId, UserId])
  end),
  {ok, State}.



request_probe(From, Method, Arg, #persistent{sent_requests = Reqs, last_req_num = N} = State) ->
  ReqId = N+1,
  Reqs1 = maps:put(ReqId, {Method, From}, Reqs),
  {ok, State1} = send_to_probe({request, ReqId, Method, Arg}, State),
  State2 = State1#persistent{sent_requests = Reqs1, last_req_num = ReqId},
  {ok, State2}.

response_from_probe(ReqId, Method, Result, #persistent{sent_requests = Reqs} = State) ->
  case maps:get(ReqId, Reqs, undefined) of
    undefined -> error({unexpected_response_reqid, ReqId, Method, Result, Reqs});

    % was requested via from protocol module
    {Method, probe_request} ->
      {ok, State1} = handle_own_request_response(Method, Result, State),

      Reqs1 = maps:remove(ReqId, Reqs),
      State2 = State1#persistent{sent_requests = Reqs1},
      {ok, State2};

    % was requested via gen_server:call
    {Method, From} ->
      gen_server:reply(From, {ok, Result}),
      Reqs1 = maps:remove(ReqId, Reqs),
      State1 = State#persistent{sent_requests = Reqs1},
      {ok, State1}
  end.



handle_own_request_response(get_user_config, _Result, State) ->
  Config = #{plug_requests => true, cowboy_requests => true},
  self() ! {probe_request, apply_config, Config},
  {ok, State};

handle_own_request_response(apply_config, ok, State) ->
  {ok, State};

handle_own_request_response(Method, Result, _State) ->
  error({unknown_probe_response, Method, Result}).



check_test_subscribers(#persistent{user_id = UserId, test_subscribers = Pids} = State) ->
  case vision_test:get_subscribers_for_user(UserId) of
    none -> {ok, State};
    {ok, Pids1} ->
      State1 = State#persistent{test_subscribers = Pids1 ++ Pids},
      {ok, State1}
  end.



send_to_probe(Term, #persistent{transport = Transport, socket = Socket} = State) ->
  % check subsribers just right before receiving/sending for now
  % rework later to receive subscribe requests
  {ok, State1} = check_test_subscribers(State),
  [Pid ! {to_probe, Term} || Pid <- State1#persistent.test_subscribers],
  ok = Transport:send(Socket, erlang:term_to_binary(Term)),
  {ok, State1}.



handle_request(get_trace_opts, _Arg, State) ->
  Opts = #{phoenix => [http_requests]},
  {ok, Opts, State};

handle_request(Method, Arg, _State) ->
  error({unknown_request_from_probe, Method, Arg}).



% in order to be able to do binary_to_existing_atom
% and binary_to_term(_, [safe]), we need to mention
% all atoms that could possible received from client
% here we go:
mention_used_atoms() ->
  [
    events,at,type,pid1,pid2,
    host,method,path,port,request_id,
    req_headers,resp_headers,resp_code,
    resp_body_size,plug,halted,
    reason,mfa
  ].
