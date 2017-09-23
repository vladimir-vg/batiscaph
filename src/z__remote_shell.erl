-module(z__remote_shell).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(shell_runner, {
  shell_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  self() ! setup,
  {ok, #shell_runner{}}.



handle_info(setup, #shell_runner{} = State) ->
  {ok, State1} = setup(State),
  {noreply, State1};

handle_info(start_tracing, #shell_runner{} = State) ->
  % trace all shell processes that spawned by runner
  Event = z__remote_collector:trace_started_event(erlang:system_time(micro_seconds), self()),
  CollectorPid = whereis(z__remote_collector),
  erlang:trace(self(), true, [procs, timestamp, set_on_spawn, {tracer, CollectorPid}]),
  CollectorPid ! Event,
  {noreply, State};

handle_info(restart_shell, #shell_runner{shell_pid = Pid} = State) when Pid =/= undefined ->
  exit(Pid, restart_shell),
  handle_info(restart_shell, State#shell_runner{shell_pid = undefined});

handle_info(restart_shell, #shell_runner{shell_pid = undefined} = State) ->
  Pid = shell:start(true, true),
  ok = gen_server:call(z__remote_collector, {ignore_pids_tracing, [Pid]}),
  {noreply, State#shell_runner{shell_pid = Pid}};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(restart_shell, _From, State) ->
  {noreply, State1} = handle_info(restart_shell, State),
  {reply, ok, State1};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



setup(State) ->
  ScenarioPid = whereis(z__remote_scenario),
  CollectorPid = whereis(z__remote_collector),
  IoServerPid = whereis(z__remote_io_server),
  ShellPid = self(),

  true = group_leader(IoServerPid, ShellPid),

  ok = gen_server:call(z__remote_collector, {ignore_pids_tracing, [ScenarioPid, CollectorPid, IoServerPid, ShellPid]}),
  ShellPid ! start_tracing,
  ShellPid ! restart_shell,

  {ok, State}.
