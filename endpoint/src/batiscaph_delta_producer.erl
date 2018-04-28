-module(batiscaph_delta_producer).
-behaviour(gen_server).
-export([start_link/1]). % supervisor callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks

-export([
  subscribe_to_delta/1,
  subscribe_to_process_info/2,
  unsubscribe_from_process_info/2
]).

% how many events to retreive for single delta chunk
-define(CHUNK_SIZE, 1000).



% This process is a way to abstract work with delta
% probably in future each instance would have such a process (share delta between clients)
% and websockets will subscribe to them (one websocket to many delta processes)



-record(delta_subscriber, {
  id :: {InstanceId :: binary(), Subscriber :: pid()},
  process_info_remote_pid
}).



% interface that different trace features should implement to produce delta
-callback desired_types() -> [binary()].
% actually it might be convenient to fetch attrs
% using arrayElement(Attrs.Value, indexOf(Attrs.Key, 'path'))
-callback desired_attrs() -> [binary()].
-callback init() -> State :: any(). % generates new state
-callback consume(Event :: map(), State :: any()) -> State :: any().
% returns delta that going to be merged with deltas frm other features
-callback finalize(State :: any()) -> Delta :: map().



subscribe_to_delta(InstanceId) ->
  Spec = {InstanceId, {?MODULE, start_link, [InstanceId]}, permanent, 5000, worker, [?MODULE]},
  {ok, ProducerPid} = gen_tracker:find_or_open(delta_producers, Spec),
  ok = gen_server:call(ProducerPid, subscribe),
  ok.



subscribe_to_process_info(InstanceId, RemotePid) ->
  {ok, ProducerPid} = gen_tracker:find(delta_producers, InstanceId),
  ok = gen_server:call(ProducerPid, {subscribe_to_process_info, RemotePid}),
  ok.

unsubscribe_from_process_info(InstanceId, RemotePid) ->
  {ok, ProducerPid} = gen_tracker:find(delta_producers, InstanceId),
  ok = gen_server:call(ProducerPid, {unsubscribe_from_process_info, RemotePid}),
  ok.



% sub_id is useful to start query from specific event
-type at() ::
  {Sec :: non_neg_integer(), Mcs :: non_neg_integer()}
| {Sec :: non_neg_integer(), Mcs :: non_neg_integer(), SubId :: non_neg_integer()}.

-record(chunk, {
  from :: at(),
  to :: at(),
  delta = #{} :: map() % map that was produced out of from-to events
}).



-record(delta, {
  % ws_pid,
  instance_id

  % should be kept sorted by 'from'
  % chunks not supposed to intersect
  % delta_chunks
}).



start_link(InstanceId) ->
  gen_server:start_link(?MODULE, [InstanceId], []).

init([InstanceId]) ->
  self() ! delta_for_old_subscribers,
  {ok, #delta{instance_id = InstanceId}}.



% 
% handle_info({delta_query, chunk_from_now}, #delta{ws_pid = Pid} = State) ->
%   {ok, Result, State1} = delta_chunk_from_now(State),
%   Pid ! {delta_query_result, Result},
%   {noreply, State1};

handle_info(check_subscribers, State) ->
  {ok, State1} = check_subscribers(State),
  {noreply, State1};

handle_info(delta_for_old_subscribers, State) ->
  % if process crashed, then we should send new delta
  % to subscribers in case if new events appeared when
  % producer was down
  {ok, State1} = delta_for_old_subscribers(State),
  {noreply, State1};

handle_info(new_events_saved, State) ->
  % when new events arrived, we should fetch
  % new events from last time and send to subscribers
  {ok, State1} = send_recent_delta_for_subscribers(State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(subscribe, {Pid, _Ref}, State) ->
  {ok, State1} = subscribe_to_delta1(Pid, State),
  {reply, ok, State1};

handle_call({subscribe_to_process_info, RemotePid}, {Pid, _Ref}, State) ->
  {ok, State1} = subscribe_to_process_info1(Pid, RemotePid, State),
  {reply, ok, State1};

handle_call({unsubscribe_from_process_info, RemotePid}, {Pid, _Ref}, State) ->
  {ok, State1} = unsubscribe_from_process_info1(Pid, RemotePid, State),
  {reply, ok, State1};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%
%
%



subscribe_to_delta1(Pid, #delta{instance_id = InstanceId} = State) when is_pid(Pid) ->
  ets:insert(delta_subscribers, #delta_subscriber{id = {InstanceId, Pid}}),
  {ok, Delta, State1} = delta_chunk_from_now(State),
  Pid ! {delta_query_result, Delta},
  {ok, State1}.



subscribe_to_process_info1(Pid, RemotePid, #delta{instance_id = InstanceId} = State) ->
  case gen_tracker:find(probes, InstanceId) of
    undefined -> {ok, State};
    {ok, ProbePid} ->
      % update delta_subscriber record
      % send subscribe to remote probe
      true = ets:update_element(delta_subscribers, {InstanceId, Pid}, [{#delta_subscriber.process_info_remote_pid, RemotePid}]),
      ok = batiscaph_probe_protocol:send_to_remote(ProbePid, {ensure_subscribed_to_process_info, RemotePid}),
      {ok, State}
  end.

unsubscribe_from_process_info1(Pid, _RemotePid, #delta{instance_id = InstanceId} = State) ->
  % update delta_subscriber record
  % send unsubscribe to remote probe if no other subscribers left
  true = ets:update_element(delta_subscribers, {InstanceId, Pid}, [{#delta_subscriber.process_info_remote_pid, undefined}]),
  self() ! check_subscribers,
  {ok, State}.



check_subscribers(State) ->
  % TODO: fetch current subscribers, remove dead
  % unsubscribe from process_info if need
  {ok, State}.



delta_for_old_subscribers(#delta{instance_id = InstanceId} = State) ->
  {ok, Delta, State1} = delta_chunk_from_now(State),

  Pids = ets:match(delta_subscribers, #delta_subscriber{id = {InstanceId, '$1'}, _ = '_'}),
  lists:foreach(fun ([P]) ->
    P ! {delta_query_result, Delta}
  end, Pids),

  {ok, State1}.



send_recent_delta_for_subscribers(State) ->
  % for now delta is not saved,
  % simply recalculate from scratch and send it
  delta_for_old_subscribers(State).



delta_chunk_from_now(#delta{instance_id = Id} = State) ->
  % TODO: observe existing chunks
  % get already fetch intervals
  % correct our request by it.
  % Do not intersect

  % do expect that events are sorted DESC
  {ok, Events} = batiscaph_clk_events:select_events(#{
    instance_id => Id, types => delta_types(), attrs => delta_attrs(),
    earlier_than => now, limit => ?CHUNK_SIZE
  }),

  case Events of
    [] -> {ok, #{}, State};
    _ ->
      % lager:info("----------------------------------"),
      % [lager:info("e: ~p", [E]) || E <- Events],
      % lager:info("----------------------------------"),
      DeltaChunk = produce_delta_chunk(Events),
      #chunk{delta = Result} = DeltaChunk,
      % TODO: insert new chunk into chunks list
      % in proper position
      {ok, Result, State}
  end.



produce_delta_chunk([#{<<"At">> := At, <<"SubId">> := SubId} | _] = Events) ->
  From = {At div (1000*1000), At rem (1000*1000), SubId},
  State = delta_init(),
  produce_delta_chunk(Events, #chunk{from = From}, State).

% last event
produce_delta_chunk([#{<<"At">> := At, <<"SubId">> := SubId} = E], Chunk, State) ->
  To = {At div (1000*1000), At rem (1000*1000), SubId},
  State1 = delta_consume(E, State),
  Delta = delta_finalize(State1),
  Chunk#chunk{to = To, delta = Delta};

produce_delta_chunk([E | Events], Chunk, State) ->
  State1 = delta_consume(E, State),
  produce_delta_chunk(Events, Chunk, State1).



delta_types() ->
  lists:usort(lists:flatmap(fun (Mod) ->
    Mod:desired_types()
  end, delta_modules())).

delta_attrs() ->
  lists:usort(lists:flatmap(fun (Mod) ->
    Mod:desired_attrs()
  end, delta_modules())).

delta_modules() ->
  [
    batiscaph_delta_plug, batiscaph_delta_cowboy, batiscaph_delta_procs,
    batiscaph_delta_shell, batiscaph_delta_process_info
  ].

delta_init() ->
  lists:foldl(fun (Mod, Acc) ->
    Acc#{Mod => Mod:init()}
  end, #{}, delta_modules()).

delta_consume(E, State) ->
  % lager:info("consume: ~p", [maps:get(<<"Type">>, E)]),
  % lager:info("state1: ~p~n~n", [State]),
  % State1 =
  maps:map(fun
    (Mod, FeatureState) -> Mod:consume(E, FeatureState)
  end, State).
  % lager:info("state2: ~p~n~n", [State1]),
  % State1.

delta_finalize(State) ->
  maps:fold(fun
    (Mod, FeatureState, Acc) -> maps:merge(Acc, Mod:finalize(FeatureState))
  end, #{}, State).
