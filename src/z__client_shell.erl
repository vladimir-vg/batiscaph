-module(z__client_shell).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([shell_context/1]).

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
  % TODO: would be better just to remember trace event for root
  % and use timestamp of the first event for it
  % this way we wouldn't worry about giving misinformation,
  % because in current there are few milliseconds when event about tracing is made, but no tracing is enabled
  % [E] = z__client_scenario:trace_started_events(erlang:system_time(micro_seconds), self()),
  z__client_scenario:trace_pid(self(), #{set_on_spawn => true}),
  % CollectorPid = whereis(z__client_collector),
  % erlang:trace(self(), true, [procs, timestamp, set_on_spawn, {tracer, CollectorPid}]),
  % CollectorPid ! E,
  {noreply, State};

handle_info(restart_shell, #shell_runner{shell_pid = Pid} = State) when Pid =/= undefined ->
  exit(Pid, restart_shell),
  handle_info(restart_shell, State#shell_runner{shell_pid = undefined});

handle_info(restart_shell, #shell_runner{shell_pid = undefined} = State) ->
  {ok, State1} = restart_shell(State),
  {noreply, State1};

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #shell_runner{shell_pid = Pid} = State) ->
  {ok, State1} = shell_died(State),
  {noreply, State1};
  

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
  ScenarioPid = whereis(z__client_scenario),
  CollectorPid = whereis(z__client_collector),
  IoServerPid = whereis(z__client_io_server),
  ShellPid = self(),

  true = group_leader(IoServerPid, ShellPid),

  ok = gen_server:call(z__client_collector, {ignore_pids_tracing, [ScenarioPid, CollectorPid, IoServerPid, ShellPid]}),
  ShellPid ! start_tracing,
  ShellPid ! restart_shell,

  {ok, State}.



restart_shell(#shell_runner{shell_pid = undefined} = State) ->
  Pid = shell:start(true, true),

  E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
    <<"pid">> => z__client_scenario:format_term(Pid),
    <<"type">> => <<"context_start">>,
    <<"context">> => shell_context(Pid),
    <<"lines">> => <<>>
  }),
  % for context_stop event generation
  erlang:monitor(process, Pid),

  ok = gen_server:call(z__client_collector, {ignore_pids_tracing, [Pid]}),
  z__client_collector ! {events, [E]},

  ok = gen_server:call(z__client_collector, {set_shell_pid, Pid}),

  {ok, State#shell_runner{shell_pid = Pid}}.



shell_died(#shell_runner{shell_pid = Pid} = State) ->
  E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
    <<"pid">> => z__client_scenario:format_term(Pid),
    <<"type">> => <<"context_stop">>,
    <<"context">> => shell_context(Pid)
  }),
  z__client_collector ! {events, [E]},
  {ok, State#shell_runner{shell_pid = undefined}}.



shell_context(Pid) ->
  iolist_to_binary([<<"shell_context_">>, pid_to_list(Pid)]).
