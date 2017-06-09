-module(es_slave).
-behaviour(gen_server).
-export([start/0]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SOCKET_OPEN_TIMEOUT, 10000).
-define(HEADERS_RECEIVE_TIMEOUT, 10000).


-record(slave, {
  id,
  master_port,
  socket
}).



start() ->
  MasterPort = list_to_integer(os:getenv("MASTER_PORT")),
  Id = os:getenv("SHELL_SESSION_ID"),

  {ok, Pid} = gen_server:start_link(?MODULE, [MasterPort, Id], []),

  % if gen_server died then stop whole node
  unlink(Pid),
  MRef = erlang:monitor(process, Pid),
  receive
    {'DOWN', MRef, process, Pid, Reason} ->
      io:format("Slave ~p died due to ~p\n", [Id, Reason]),
      timer:sleep(1000),
      init:stop()
  end,

  ok.



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(MasterPort, Id) ->
  gen_server:start_link(?MODULE, [MasterPort, Id], []).

init([MasterPort, Id]) ->
  self() ! connect_to_master,
  {ok, #slave{master_port = MasterPort, id = Id}}.



handle_info(connect_to_master, State) ->
  {ok, State1} = connect_to_master(State),
  {noreply, State1};


handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%%%%%%%%%



connect_to_master(#slave{socket = undefined, master_port = Port, id = Id} = State) ->
  Opts = [binary, {packet, http}, {send_timeout, ?SOCKET_OPEN_TIMEOUT}],
  {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts, ?SOCKET_OPEN_TIMEOUT),
  gen_tcp:send(Socket, [
    "CONNECT /api/v0?id=", Id, " HTTP/1.1\r\n",
    "Host: 127.0.0.1\r\n",
    "Connection: Upgrade\r\n",
    "User-Agent: ESpace Slave\r\n",
    "Upgrade: application/espace-v0\r\n",
    "\r\n"
  ]),
  ok = receive_initial_info(Socket),

  % okay, from we stream binary packets
  inet:setopts(Socket, [{packet,4}]),
  gen_tcp:send(Socket, <<"Hello there!">>),

  {ok, State}.

receive_initial_info(Socket) ->
  receive
    {http,_,{http_response, _, 101, _}} ->
      inet:setopts(Socket, [{active,once}]),
      receive_initial_info(Socket);
    {http,_,{http_response,_,Code,_}} ->
      {error, {unexpected_http_code, Code}};
    {http,_,{http_header,_,_Header,_,_Value}} ->
      inet:setopts(Socket, [{active,once}]),
      receive_initial_info(Socket);
    {http,_,http_eoh} -> ok
  after ?HEADERS_RECEIVE_TIMEOUT -> {error, receive_timeout}
  end.