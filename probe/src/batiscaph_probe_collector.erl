-module(batiscaph_probe_collector).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([ensure_basic_tracing/1]).

-record(collector, {
  feature_states = #{}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #collector{}}.



handle_info(TraceEvent, State) when element(1, TraceEvent) =:= trace_ts ->
  % io:format("~p~n", [TraceEvent]),
  {ok, Events, State1} = match_and_consume_event(TraceEvent, State),
  ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, Events}}),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call({apply_config, Config}, _From, State) ->
  {ok, State1} = setup_tracing(Config, State),
  {reply, ok, State1};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%
%
%



ensure_basic_tracing(Pid) ->
  try erlang:trace(Pid, true, [procs, timestamp, set_on_spawn, {tracer, whereis(batiscaph_probe_collector)}]) of
    1 ->
      TraceEvents = batiscaph_probe_feature_procs:tracing_started_event(Pid, erlang:now()),
      {ok, TraceEvents}
  catch
    error:badarg -> {ok, []} % if process already died, just ignore
  end.



setup_tracing(#{} = Opts, #collector{} = State) ->
  State1 = State,
  % {ok, State1} = setup_trace_feature(batiscaph_probe_feature_plug, Opts, State),
  {ok, State2} = setup_trace_feature(batiscaph_probe_feature_cowboy, Opts, State1),
  {ok, State3} = setup_trace_feature(batiscaph_probe_feature_procs, Opts, State2),
  {ok, State4} = setup_trace_feature(batiscaph_probe_feature_lager, Opts, State3),
  {ok, State4}.



setup_trace_feature(Module, Opts, #collector{feature_states = FeatureStates} = State) ->
  case Module:try_init(Opts) of
    skip -> {ok, State};
    {ok, FeatureState} ->
      State1 = State#collector{feature_states = maps:put(Module, FeatureState, FeatureStates)},
      {ok, State1}
  end.



match_and_consume_event(TraceEvent, #collector{feature_states = FeatureStates0} = State) ->
  {Events, FeatureStates} = maps:fold(fun
    (Module, FState, {Events1, FeatureStates1}) ->
      case Module:match_trace_event(TraceEvent, FState) of
        false -> {Events1, maps:put(Module, FState, FeatureStates1)};
        true ->
          {ok, Events2, FState1} = Module:consume(TraceEvent, FState),
          {Events2 ++ Events1, maps:put(Module, FState1, FeatureStates1)}
      end
  end, {[], #{}}, FeatureStates0),
  State1 = State#collector{feature_states = FeatureStates},
  {ok, Events, State1}.
