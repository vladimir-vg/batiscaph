-module(batiscaph_probe_session).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([mention_used_atoms/0]).
-define(EVENTS_FLUSH_INTERVAL, 100).



-record(session, {
  socket,
  sent_requests = #{}, % non_neg_integer() => atom()
  last_req_num = 0,

  events_acc = [],
  events_send_timer
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  % not all applications are be loaded on this stage
  % wait a bit before sending summary to be able to determine dependency
  erlang:send_after(1000, self(), init),
  {ok, #session{}}.



handle_info(init, State) ->
  % case application:get_env(batiscaph_probe, access_key) of
  %   undefined ->
  %     ok = send_guest_summary(),
  %     {noreply, State};
  % 
  %   {ok, AccessKey} ->
  % end;
  {ok, State1} = open_persistent_connection(<<"0:0:0">>, State),
  {noreply, State1};

handle_info(send_summary, #session{} = State) ->
  ok = send_to_service(get_summary_to_send(), State),
  {noreply, State};

handle_info({to_service, Term}, #session{} = State) ->
  ok = send_to_service(Term, State),
  {noreply, State};

handle_info(flush_events, #session{} = State) ->
  {ok, State1} = flush_events(State),
  {noreply, State1};

handle_info({tcp, Socket, Data}, #session{socket = Socket} = State) ->
  inet:setopts(Socket, [{active, once}]),
  Term = erlang:binary_to_term(Data, [safe]),
  {ok, State1} = handle_message_from_service(Term, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



% reply to this call will be sent on arrival
% do not block, proceed to take other requests
handle_call({request, Method, Arg}, From, #session{} = State) ->
  {ok, State1} = request_service(From, Method, Arg, State),
  {noreply, State1};

handle_call({queue_for_send, {events, Events}}, _From, #session{} = State) ->
  {ok, State1} = maybe_send_events(Events, State),
  {reply, ok, State1};

handle_call({queue_for_send, Message}, _From, #session{} = State) ->
  ok = send_to_service(Message, State),
  {reply, ok, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_message_from_service({request, ReqId, Method, Arg}, State) ->
  {ok, Result, State1} = handle_request(Method, Arg, State),
  % lager:info("probe request ~p ~p ~p -> ~p", [ReqId, Method, Arg, Result]),
  ok = send_to_service({response, ReqId, Method, Result}, State1),
  {ok, State1};

handle_message_from_service({response, ReqId, Method, Result}, State) ->
  {ok, State1} = response_from_service(ReqId, Method, Result, State),
  {ok, State1};

handle_message_from_service({shell_input, Text}, State) ->
  batiscaph_probe_io_server ! {input, Text},
  {ok, State};

handle_message_from_service({ensure_subscribed_to_process_info, PidBin}, State) ->
  Pid = erlang:list_to_pid(erlang:binary_to_list(PidBin)),
  {ok, Events} = batiscaph_probe_collector:ensure_basic_tracing(Pid),
  {ok, State1} = maybe_send_events(Events, State),
  ok = gen_server:call(batiscaph_probe_subs_worker, {ensure_subscribed_to_process_info, Pid}),
  {ok, State1};

handle_message_from_service(Message, State) ->
  io:format("unknown message from service ~p~n", [Message]),
  {ok, State}.



handle_request(get_user_config, _Arg, State) ->
  % trace related options specified by user
  % that service should take into consideration
  % before producing opts for tracing
  {ok, #{}, State};

handle_request(apply_config, Config, State) ->
  % send new config via gen_server:call to collector
  ok = gen_server:call(batiscaph_probe_collector, {apply_config, Config}),
  {ok, ok, State};

handle_request(ensure_shell_started, Arg, State) ->
  ensure_shell_started(Arg, State);

handle_request(Method, Arg, _State) ->
  error({unknown_request_from_service, Method, Arg}).



%
%
%



ensure_shell_started(_Arg, State) ->
  case [1 || {batiscaph_probe_shell,_,_,_} <- supervisor:which_children(batiscaph_probe_sup)] of
    [1] -> {ok, [], State};
    [] ->
      ChildSpec = {batiscaph_probe_shell, {batiscaph_probe_shell, start_link, []}, permanent, 5000, worker, [batiscaph_probe_shell]},
      {ok, _} = supervisor:start_child(batiscaph_probe_sup, ChildSpec),
      {ok, [], State}
  end.



% send_guest_summary() ->
%   Info = get_summary_to_send(),
%   ok = send_guest_info(Info),
%   ok.

get_summary_to_send() ->
  Apps = batiscaph_probe_util:apps_have_dependency(batiscaph_probe),
  DependencyIn = [{atom_to_binary(AppId, latin1), list_to_binary(Ver)} || {AppId, _Desc, Ver} <- Apps],
  {ok, Version} = application:get_key(batiscaph_probe, vsn),
  {summary_info, #{
    probe_version => list_to_binary(Version),
    dependency_in => DependencyIn
  }}.



% send_guest_info(Info) ->
%   % for now only plain HTTP is supported
%   {ok, <<"http://", _/binary>> = Url} = application:get_env(batiscaph_probe, endpoint_url),
%   Content = erlang:term_to_binary(Info),
%   Headers = [],
%   Request = {binary_to_list(Url), Headers, "application/batiscaph-guest-info-v0", Content},
%   case httpc:request(post, Request, [], []) of
%     {ok, {{_, 200, _}, _Headers, _Body}} -> ok
%   end.



open_persistent_connection(AccessKey, State) ->
  {ok, Url} = application:get_env(batiscaph_probe, endpoint_url),
  {ok, {http, "", Host, Port, Path, _Anchor}} = http_uri:parse(binary_to_list(Url)),

  {ok, Token} = generate_token(AccessKey),

  Opts = [{active, once}, {keepalive, true}, {packet, http}],
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts, 10000),
  ok = gen_tcp:send(Socket, [
    "CONNECT ", Path, " HTTP/1.1\r\n",
    "Host: ", Host, "\r\n",
    "Connection: Upgrade\r\n",
    "Upgrade: application/batiscaph-persistent-v0\r\n",
    "X-Runtime-Batiscaph-Token: ", Token, "\r\n",
    "\r\n"
  ]),

  ok = read_out_protocol_switch(Socket),
  inet:setopts(Socket, [binary, {packet, 4}, {active, once}]),
  State1 = State#session{socket = Socket},

  self() ! send_summary,

  {ok, State1}.



generate_token(AccessKey) ->
  [<<"0">>, UserId, SecretKey] = binary:split(AccessKey, <<":">>, [global]),
  Salt = batiscaph_probe_util:bin_to_hex(crypto:strong_rand_bytes(8)),
  Hash = batiscaph_probe_util:bin_to_hex(crypto:hash(sha, [UserId, "/", SecretKey, "/", Salt])),
  Token = <<UserId/binary, ":", Salt/binary, ":", Hash/binary>>,
  {ok, Token}.



read_out_protocol_switch(Socket) ->
  read_out_protocol_switch(Socket, #{}).

read_out_protocol_switch(Socket, State) ->
  receive
    {tcp_closed, Socket} -> {error, unexpected_close};

    {http, Socket, {http_response, _, Code, _}} ->
      inet:setopts(Socket, [{active, once}]),
      read_out_protocol_switch(Socket, State#{resp_code => Code});

    {http, Socket, {http_header, _, 'Connection', _, "Upgrade"}} ->
      inet:setopts(Socket, [{active, once}]),
      read_out_protocol_switch(Socket, State);

    {http, Socket, {http_header, _, 'Upgrade', _, Proto}} ->
      inet:setopts(Socket, [{active, once}]),
      read_out_protocol_switch(Socket, State#{proto => Proto});

    % {http, Socket, {http_header, _, "X-Runtime-Batiscaph-Instance-Id", _, InstanceId}} ->
    %   io:format("Connected as ~p~n", [InstanceId]),
    %   inet:setopts(Socket, [{active, once}]),
    %   read_out_protocol_switch(Socket, State);

    {http, Socket, http_eoh} ->
      Code = maps:get(resp_code, State, undefined),
      Proto = maps:get(proto, State, undefined),
      case {Code, Proto} of
        {101, "application/batiscaph-persistent-v0"} -> ok;
        _ -> {error, {failed_to_switch, State}}
      end

    % X ->
    %   io:format("read_out_protocol_switch ~p~n", [X]),
    %   inet:setopts(Socket, [{active, once}]),
    %   read_out_protocol_switch(Socket)
  after 10000 ->
    % io:format("process: ~p~n", [erlang:process_info(self(), messages)]),
    {error, timeout}
  end.



send_to_service(Term, #session{socket = Socket}) ->
  ok = gen_tcp:send(Socket, erlang:term_to_binary(Term)),
  ok.



request_service(From, Method, Arg, #session{sent_requests = Reqs, socket = Socket, last_req_num = N} = State) ->
  ReqId = N+1,
  Reqs1 = maps:put(ReqId, {Method, From}, Reqs),
  ok = gen_tcp:send(Socket, erlang:term_to_binary({request, ReqId, Method, Arg})),
  State1 = State#session{sent_requests = Reqs1, last_req_num = ReqId},
  {ok, State1}.



response_from_service(ReqId, Method, Result, #session{sent_requests = Reqs} = State) ->
  case maps:get(ReqId, Reqs, undefined) of
    undefined -> error({unexpected_response_reqid, ReqId, Method, Result, Reqs});
    {Method, From} ->
      gen_server:reply(From, {ok, Result}),
      Reqs1 = maps:remove(ReqId, Reqs),
      State1 = State#session{sent_requests = Reqs1},
      {ok, State1}
  end.



% buffer events, do not send immediately
maybe_send_events(Events, #session{events_send_timer = undefined, events_acc = Acc} = State) ->
  Timer = erlang:send_after(?EVENTS_FLUSH_INTERVAL, self(), flush_events),
  State1 = State#session{events_send_timer = Timer, events_acc = Events ++ Acc},
  {ok, State1};

% already set a timer, wait for it to fire
maybe_send_events(Events, #session{events_acc = Acc} = State) ->
  State1 = State#session{events_acc = Events ++ Acc},
  {ok, State1}.



flush_events(#session{events_send_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  flush_events(State#session{events_send_timer = undefined});

flush_events(#session{events_send_timer = undefined, events_acc = Events} = State) ->
  ok = send_to_service({events, Events}, State),
  State1 = State#session{events_acc = []},
  {ok, State1}.



mention_used_atoms() ->
  [
    request,response,
    get_user_config,apply_config,
    plug_requests,cowboy_requests,
    ensure_subscribed_to_process_info,
    shell_input
  ].
