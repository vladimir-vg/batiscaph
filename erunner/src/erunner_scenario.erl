-module(erunner_scenario).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SOCKET_OPEN_TIMEOUT, 10000).
-define(HEADERS_RECEIVE_TIMEOUT, 10000).

-record(runner, {
  id,
  master_port,
  socket,

  io_server_pid,
  collector_pid,
  shell_runner_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(MasterPort, Id) ->
  gen_server:start_link(?MODULE, [MasterPort, Id], []).

init([MasterPort, Id]) ->
  self() ! connect_to_master,
  {ok, #runner{master_port = MasterPort, id = Id}}.



handle_info(connect_to_master, State) ->
  {ok, State1} = connect_to_master(State),
  {noreply, State1};


handle_info({tcp, Socket, Binary}, #runner{socket = Socket} = State) ->
  {ok, State1} = handle_runner_message(erlang:binary_to_term(Binary), State),
  inet:setopts(Socket, [{active,once}]),
  {noreply, State1};

handle_info(Msg, State) ->
  io:format("from slave unknown msg: ~p~n", [Msg]),
  {noreply, State}.
  % {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%%%%%%%%%



connect_to_master(#runner{socket = undefined, master_port = Port, id = Id} = State) ->
  Opts = [binary, {packet, http}, {send_timeout, ?SOCKET_OPEN_TIMEOUT}],
  {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts, ?SOCKET_OPEN_TIMEOUT),
  gen_tcp:send(Socket, [
    "CONNECT /api/v0?id=", Id, " HTTP/1.1\r\n",
    "Host: 127.0.0.1\r\n",
    "Connection: Upgrade\r\n",
    "User-Agent: ERunner\r\n",
    "Upgrade: application/espace-v0\r\n",
    "\r\n"
  ]),
  ok = receive_initial_info(Socket),

  % okay, from here we stream binary packets
  inet:setopts(Socket, [{active,once},{packet,4}]),
  gen_tcp:send(Socket, erlang:term_to_binary({events, [#{key => <<"this is event">>, at => 123}]})),

  {ok, State#runner{socket = Socket}}.

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



%%%%%%%%%



handle_runner_message({shell_input, Input}, #runner{io_server_pid = IoServerPid} = State) ->
  io:format("feed to io server: ~p\n\n", [Input]),
  IoServerPid ! {input, binary_to_list(Input)},
  {ok, State};

handle_runner_message(start_shell, #runner{id = Id} = State) ->
  Self = self(),
  {ok, CollectorPid} = es_collector:start_link(<<Id/binary, ".csv">>),
  {ok, IoServerPid} = es_shell_io_server:start_link(#{collector => CollectorPid, parent => Self, stale_timeout => 5000}),
  {ok, ShellPid} = es_shell_runner:start_link(CollectorPid),

  % capture all stdin/stdout io for shell runner process and its children
  group_leader(IoServerPid, ShellPid),

  ok = gen_server:call(CollectorPid, {ignore_pids_tracing, [self(), CollectorPid, IoServerPid, ShellPid]}),
  ok = gen_server:call(ShellPid, start_tracing),

  ShellPid ! restart_shell,

  {ok, State#runner{io_server_pid = IoServerPid, collector_pid = CollectorPid, shell_runner_pid = ShellPid}}.