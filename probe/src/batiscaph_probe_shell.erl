-module(batiscaph_probe_shell).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-record(shell, {
  io_server_pid,
  shell_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  self() ! init,
  % start io server right during sync creation of shell process
  % this will allow to receive shell_input event if shell didn't start yet
  {ok, IoServerPid} = batiscaph_probe_io_server:start_link(),
  {ok, #shell{io_server_pid = IoServerPid}}.



handle_info(init, State) ->
  {ok, State1} = init0(State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%
%
%



init0(#shell{io_server_pid = IoServerPid} = State) ->
  true = group_leader(IoServerPid, self()),

  erlang:trace(self(), true, [procs, timestamp, set_on_spawn, {tracer, whereis(batiscaph_probe_collector)}]),

  ShellPid = shell:start(true, true),
  State1 = State#shell{io_server_pid = IoServerPid, shell_pid = ShellPid},

  {ok, State1}.