-module(erltv_shell_runner).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(shell_runner, {
  shell_pid,
  collector_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(CollectorPid) ->
  gen_server:start_link(?MODULE, [CollectorPid], []).

init([CollectorPid]) ->
  {ok, #shell_runner{collector_pid = CollectorPid}}.



handle_info(restart_shell, #shell_runner{shell_pid = Pid} = State) when Pid =/= undefined ->
  exit(Pid, restart_shell),
  handle_info(restart_shell, State#shell_runner{shell_pid = undefined});

handle_info(restart_shell, #shell_runner{shell_pid = undefined, collector_pid = CollectorPid} = State) ->
  Pid = shell:start(false, true),
  ok = gen_server:call(CollectorPid, {ignore_pids_tracing, [Pid]}),
  {noreply, State#shell_runner{shell_pid = Pid}};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(start_tracing, _From, #shell_runner{collector_pid = CollectorPid} = State) ->
  % trace all shell processes that spawned by runner
  erlang:trace(self(), true, [procs, send, timestamp, set_on_spawn, {tracer, CollectorPid}]),
  {reply, ok, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
