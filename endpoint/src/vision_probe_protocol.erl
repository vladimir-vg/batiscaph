-module(vision_probe_protocol).
-behaviour(gen_server).
-export([upgrade/4]). % cowboy callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks



-record(persistent, {
  socket,
  transport,
  user_id,
  test_subscriber
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
  self() ! check_test_subscribers,

  % take this socket out of pool
  % more details in ranch official documentation
  Ref = proplists:get_value(listener, Env),
  ok = ranch:remove_connection(Ref),

  Headers = [{<<"upgrade">>, <<"application/vision-persistent-v0">>}],
  {ok, Req1} = cowboy_req:upgrade_reply(101, Headers, Req),
  [Socket, Transport] = cowboy_req:get([socket, transport], Req1),
  Transport:setopts(Socket, [{active,once},{packet,4}]),

  % cowboy sends this message to self to indicate that sent was successful
  % read it out from mailbox
  receive {cowboy_req,resp_sent} -> ok
  after 1000 -> error(failed_cowboy_upgrade_reply)
  end,


  % now we turning this process into gen_server
  % add necessary dict values to make it happy
  put('$ancestors', [self()]),
  State = #persistent{socket = Socket, transport = Transport, user_id = UserId},
  gen_server:enter_loop(?MODULE, [], State).



%
%
%



init(_) ->
  error(not_supposed_to_be_started_directly).



handle_info(check_test_subscribers, State) ->
  {ok, State1} = check_test_subscribers(State),
  {noreply, State1};

handle_info({tcp, Socket, Data}, #persistent{socket = Socket, transport = Transport} = State) ->
  Transport:setopts(Socket, [{active,once}]),
  {ok, State1} = handle_data_from_probe(Data, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_data_from_probe(Data, #persistent{test_subscriber = Pid} = State) ->
  Term = erlang:binary_to_term(Data),
  if is_pid(Pid) -> Pid ! {from_probe, Term}, ok;
    true -> ok
  end,

  {ok, State1} = handle_message_from_probe(Term, State),
  {ok, State1}.



handle_message_from_probe({summary_info, Info}, State) ->
  lager:info("got summary info: ~p", [Info]),
  {ok, State};

handle_message_from_probe(Message, State) ->
  lager:info("got unknown message from probe: ~p", [Message]),
  {ok, State}.



%
%
%



check_test_subscribers(#persistent{user_id = UserId} = State) ->
  case vision_test:get_subscriber_for_user(UserId) of
    none -> lager:info("no test subscr ~p", [ets:tab2list(test_subscriptions)]), {ok, State};
    {ok, Pid} ->
      lager:info("got test subscriber: ~p", [Pid]),
      State1 = State#persistent{test_subscriber = Pid},
      {ok, State1}
  end.
