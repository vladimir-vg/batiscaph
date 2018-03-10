-module(vision_delta_producer).
-behaviour(gen_server).
-export([start_link/2]). % supervisor callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks

% how many events to retreive for single delta chunk
-define(CHUNK_SIZE, 100).



% This process is a way to abstract work with delta
% probably in future each instance would have such a process (share delta between clients)
% and websockets will subscribe to them (one websocket to many delta processes)



% interface that different trace features should implement to produce delta
-callback desired_types() -> [binary()].
% actually it might be convenient to fetch attrs
% using arrayElement(Attrs.Value, indexOf(Attrs.Key, 'path'))
% -callback desired_attrs() -> [binary()].
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
    instance_id => Id, types => vision_delta_phoenix:desired_types(),
    earlier_than => now, limit => ?CHUNK_SIZE
  }),

  DeltaChunk = produce_delta_chunk(Events),
  #chunk{delta = Result} = DeltaChunk,
  % TODO: insert new chunk into chunks list
  % in proper position
  {ok, Result, State}.



produce_delta_chunk([#{<<"At">> := At, <<"SubId">> := SubId} | _] = Events) ->
  From = {At div (1000*1000), At rem (1000*1000), SubId},
  State = vision_delta_phoenix:init(),
  produce_delta_chunk(Events, #chunk{from = From}, State).

% last event
produce_delta_chunk([#{<<"At">> := At, <<"SubId">> := SubId} = E], Chunk, State) ->
  To = {At div (1000*1000), At rem (1000*1000), SubId},
  State1 = vision_delta_phoenix:consume(E, State),
  Delta = vision_delta_phoenix:finalize(State1),
  Chunk#chunk{to = To, delta = Delta};

produce_delta_chunk([E | Events], Chunk, State) ->
  State1 = vision_delta_phoenix:consume(E, State),
  produce_delta_chunk(Events, Chunk, State1).
