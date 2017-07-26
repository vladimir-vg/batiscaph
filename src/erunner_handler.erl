-module(erunner_handler).
-export([init/3, upgrade/4, handle_info/2, terminate/2]).

-define(BUFFER_FLUSH_SIZE, 2*1024).
-define(BUFFER_FLUSH_INTERVAL, 1000).



-record(erunner_handler, {
  id :: binary(),
  transport :: atom(),
  socket,
  graph_producer_pid :: pid()
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

  true = gproc:reg({n, l, {erunner, Id}}),

  {ok, GrapProducer} = graph_builder:start_link(Id),

  lager:info("Opened connection with ~s", [Id]),

  gen_server:enter_loop(?MODULE, [], #erunner_handler{
    id = Id,
    transport = Transport,
    socket = Socket,
    graph_producer_pid = GrapProducer
  }).



handle_info(shell_restart, #erunner_handler{transport = Transport, socket = Socket} = State) ->
  lager:info("restarting shell"),
  Transport:send(Socket, erlang:term_to_binary(shell_restart)),
  {noreply, State};

handle_info({shell_input, Input}, #erunner_handler{transport = Transport, socket = Socket} = State) ->
  lager:info("sending input: ~p", [Input]),
  Transport:send(Socket, erlang:term_to_binary({shell_input, Input})),
  {noreply, State};

handle_info({store_module, Name, Body}, #erunner_handler{transport = Transport, socket = Socket} = State) ->
  lager:info("storing module: ~p with ~p bytes body", [Name, byte_size(Body)]),
  Transport:send(Socket, erlang:term_to_binary({store_module, Name, Body})),
  {noreply, State};

handle_info({tcp, Socket, Binary}, #erunner_handler{transport = Transport} = State) ->
  Transport:setopts(Socket, [{active,once}]),
  {ok, State1} = handle_runner_message(erlang:binary_to_term(Binary), State),
  {noreply, State1};

handle_info({tcp_closed, Socket}, #erunner_handler{socket = Socket, id = Id} = State) ->
  lager:info("Closed connection with ~s", [Id]),
  {stop, normal, State};

handle_info(Message, State) ->
  lager:error("Unknown message: ~p", [Message]),
  {noreply, State}.



%%%%%%%%%%%%%%%%



handle_runner_message({events, Events}, #erunner_handler{id = Id, graph_producer_pid = ProducerPid} = State) ->
  catch gproc:send({n, l, {websocket, Id}}, {events, Events}), % try to send events if websocket is alive
  ok = es_events:store(Id, Events),
  ProducerPid ! new_events_stored,
  {ok, State}.