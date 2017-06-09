-module(es_v0_handler).
-export([init/3, upgrade/4, handle_info/2, terminate/2]).

-define(BUFFER_FLUSH_SIZE, 2*1024).
-define(BUFFER_FLUSH_INTERVAL, 1000).


-record(v0_handler, {
  id,
  transport,
  socket
}).



terminate(_,_) -> ok.

init(_,_Req,_) ->
  {upgrade, protocol, ?MODULE}.

upgrade(Req, Env, ?MODULE, []) ->
  {Qs, Req1} = cowboy_req:qs_vals(Req),
  Id = proplists:get_value(<<"id">>, Qs),

  {_, Ref} = lists:keyfind(listener, 1, Env),
  ranch:remove_connection(Ref),

  {ok, Req2} = cowboy_req:upgrade_reply(101, [{<<"upgrade">>,<<"application/espace-v0">>}], Req1),
  receive
    {cowboy_req,resp_sent} -> ok
  after
    1000 -> error(timeout_open)
  end,

  [Socket,Transport] = cowboy_req:get([socket,transport], Req2),
  Transport:setopts(Socket, [{active,once},{packet,4}]),

  % to get rid of process_was_not_started_by_proc_lib error
  put('$ancestors', [self()]),

  lager:info("Opened connection with ~s", [Id]),

  gen_server:enter_loop(?MODULE, [], #v0_handler{
    id = Id,
    transport = Transport,
    socket = Socket
  }).



handle_info({tcp, Socket, Binary}, #v0_handler{transport = Transport} = State) ->
  Transport:setopts(Socket, [{active,once}]),
  {ok, State1} = handle_runner_message(Binary, State),
  {noreply, State1};

handle_info({tcp_closed, Socket}, #v0_handler{socket = Socket, id = Id} = State) ->
  lager:info("Closed connection with ~s", [Id]),
  {stop, normal, State};

handle_info(Message, State) ->
  lager:error("Unknown message: ~p", [Message]),
  {noreply, State}.



%%%%%%%%%%%%%%%%



handle_runner_message(Binary, #v0_handler{id = Id} = State) ->
  case erlang:binary_to_term(Binary) of
    {events, Events} ->
      [Pid ! {runner_info, Id, self(), {events, Events}} || {_, Pid} <- ets:lookup(events_subscribers, Id)],
      {ok, State}
  end.
