-module(vision_delta_producer).
-behaviour(gen_server).
-export([start_link/2]). % supervisor callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks

% how many events to retreive for single delta chunk
-define(CHUNK_SIZE, 1000).



% This process is a way to abstract work with delta
% probably in future each instance would have such a process (share delta between clients)
% and websockets will subscribe to them (one websocket to many delta processes)



% TODO: it's not clear how to produce correct delta
% when start and stop of request are in different chunks
% but we can't give good id from single event
% because request_id appears only at second event
% and pid may be reused by other requests.
% There must be some kind of continuation, in deltas.
% Or just store incomplete requests separately, where their pids gonna be unique?
%
% merge callback for chunks?



% interface that different trace features should implement to produce delta
-callback desired_types() -> [binary()].
% actually it might be convenient to fetch attrs
% using arrayElement(Attrs.Value, indexOf(Attrs.Key, 'path'))
-callback desired_attrs() -> [binary()].
-callback init() -> State :: any(). % generates new state
-callback consume(Event :: map(), State :: any()) -> State :: any().
% returns delta that going to be merged with deltas frm other features
-callback finalize(State :: any()) -> Delta :: map().



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
  ws_pid,
  instance_id

  % should be kept sorted by 'from'
  % chunks not supposed to intersect
  % delta_chunks
}).



start_link(WebsocketPid, InstanceId) ->
  gen_server:start_link(?MODULE, [WebsocketPid, InstanceId], []).

init([WebsocketPid, InstanceId]) ->
  {ok, #delta{instance_id = InstanceId, ws_pid = WebsocketPid}}.




handle_info({delta_query, chunk_from_now}, #delta{ws_pid = Pid} = State) ->
  {ok, Result, State1} = delta_chunk_from_now(State),
  Pid ! {delta_query_result, Result},
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



delta_chunk_from_now(#delta{instance_id = Id} = State) ->
  % TODO: observe existing chunks
  % get already fetch intervals
  % correct our request by it.
  % Do not intersect

  % do expect that events are sorted DESC
  {ok, Events} = vision_clk_events:select_events(#{
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
  [vision_delta_plug, vision_delta_cowboy, vision_delta_procs].

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
