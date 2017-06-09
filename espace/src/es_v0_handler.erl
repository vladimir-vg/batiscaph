-module(es_v0_handler).
-export([init/3, upgrade/4, handle_info/2, terminate/2]).

-define(BUFFER_FLUSH_SIZE, 2*1024).
-define(BUFFER_FLUSH_INTERVAL, 1000).


-record(bert_handler, {
  id,
  transport,
  socket,

  buffer = <<>>,
  flush_timer
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

  self() ! flush_buffer,

  lager:info("Opened connection with ~s", [Id]),

  gen_server:enter_loop(?MODULE, [], #bert_handler{
    id = Id,
    transport = Transport,
    socket = Socket
  }).



handle_info({tcp, Socket, Binary}, #bert_handler{transport = Transport, buffer = Buffer} = State) ->
  lager:info("recv from socket: ~p", [Binary]),
  Transport:setopts(Socket, [{active,once}]),
  Buffer1 = <<Buffer/binary, Binary/binary>>,
  case byte_size(Buffer1) of
    N when N > ?BUFFER_FLUSH_SIZE ->
      {ok, State1} = process_buffer(State#bert_handler{buffer = Buffer1}),
      {noreply, State1};
    _ -> {noreply, State#bert_handler{buffer = Buffer1}}
  end;

handle_info({tcp_closed, Socket}, #bert_handler{socket = Socket, id = Id} = State) ->
  lager:info("Closed connection with ~s", [Id]),
  {stop, normal, State};



handle_info(flush_buffer, #bert_handler{flush_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  handle_info(flush_buffer, State#bert_handler{flush_timer = undefined});

handle_info(flush_buffer, #bert_handler{flush_timer = undefined} = State) ->
  {ok, State1} = process_buffer(State),
  Timer = erlang:send_after(?BUFFER_FLUSH_INTERVAL, self(), flush_buffer),
  {noreply, State1#bert_handler{flush_timer = Timer}};



handle_info(Message, State) ->
  lager:error("Unknown message: ~p", [Message]),
  {noreply, State}.



%%%%%%%%%%%%%%%%



process_buffer(State) -> {ok, State}.
