-module(batiscaph_events_mnesia).
-include_lib("stdlib/include/ms_transform.hrl").
-export([setup/0]).
-export([insert/1]).

% different select queries
-export([
  select_instances_ids/0,
  select_instances_infos_with_ids/1,
  select_all_instances_infos/0,
  select_events/1,
  select_plug_request_info/1, select_cowboy_request_info/1
]).



% This module implements same database interface as batiscaph_clk_events
% but for Mnesia.
%
% Mnesia is used only to reduce number of dependencies for setting it up for the first time.



-record(event, {
  % this field is primary key
  key :: {
    InstanceId :: binary(),
    AtSec :: non_neg_integer(),
    AtMcs :: non_neg_integer(),
    SubId :: non_neg_integer(),
    Type :: binary()
  },
  pid1 = <<>> :: binary(),
  pid2 = <<>> :: binary(),
  attrs = [] :: [{binary(), binary()}]
}).



setup() ->
  application:ensure_all_started(mnesia),
  TableOpts = [
    {attributes, record_info(fields, event)},
    {type, ordered_set}
  ],
  {atomic, ok} = mnesia:create_table(event, TableOpts),
  ok.



%
% storage callbacks
%



insert(Events) ->
  [ok = mnesia:dirty_write(event_input_map_to_record(E)) || E <- Events],
  ok.



event_input_map_to_record(E) ->
  #{
    <<"InstanceId">> := InstanceId, <<"AtSec">> := AtSec, <<"AtMcs">> := AtMcs,
    <<"Type">> := Type, <<"SubId">> := SubId
  } = E,
  Pid1 = maps:get(<<"Pid1">>, E, <<>>),
  Pid2 = maps:get(<<"Pid2">>, E, <<>>),
  Keys = maps:get(<<"Attrs.Key">>, E, []),
  Values = maps:get(<<"Attrs.Value">>, E, []),
  #event{
    key = {InstanceId, AtSec, AtMcs, SubId, Type},
    pid1 = Pid1, pid2 = Pid2,
    attrs = lists:zip(Keys, Values)
  }.



record_to_event_delta_map(E) ->
  #event{
    key = {InstanceId, AtSec, AtMcs, SubId, Type},
    pid1 = Pid1, pid2 = Pid2,
    attrs = Attrs
  } = E,
  AttrsMap = maps:from_list(Attrs),
  AttrsMap#{
    <<"InstanceId">> => InstanceId, <<"At">> => (AtSec*1000*1000 + AtMcs),
    <<"Type">> => Type, <<"SubId">> => SubId,
    <<"Pid1">> => Pid1, <<"Pid2">> => Pid2
  }.



select_instances_ids() ->
  error(not_implemented).



select_instances_infos_with_ids([]) -> {ok, []};
select_instances_infos_with_ids(_Ids) ->
  error(not_implemented).



select_all_instances_infos() ->
  Match = ets:fun2ms(fun
    (#event{key = {InstanceId, AtSec, AtMcs, _, Type}, _ = '_'})
    when Type =:= <<"0 batiscaph connection-start">>
    orelse Type =:= <<"0 batiscaph connection-stop">> ->

      {InstanceId, Type, AtSec*1000*1000 + AtMcs}
  end),

  Tuples = mnesia:dirty_select(event, Match),
  CurrentlyConnected = [Id || {Id, _Attrs} <- gen_tracker:list(probes, [])],

  Instances = lists:foldl(fun
    ({Id, <<"0 batiscaph connection-start">>, At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Map1 = Map#{<<"Connected">> => lists:member(Id, CurrentlyConnected)},
      Acc#{Id => maps:put(<<"StartedAt">>, At, Map1)};

    ({Id, <<"0 batiscaph connection-stop">>, At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Acc#{Id => maps:put(<<"StoppedAt">>, At, Map)}
  end, #{}, Tuples),

  {ok, maps:values(Instances)}.



% HACK: limit is just ignored for now
select_events(#{
  instance_id := Id, types := Types, attrs := Attrs,
  earlier_than := now, limit := _Limit
}) ->

  % TODO: it's possible to filter by type
  % but requires manual conversion Types into matchspec conditionals
  % fetch everything for now
  Match = ets:fun2ms(fun
    (#event{key = {InstanceId1, _, _, _, _}, _ = '_'} = E)
    when InstanceId1 =:= Id ->
      E
  end),

  Events = mnesia:dirty_select(event, Match),
  Events1 = lists:filtermap(fun (E) ->
    filtermap_event(Types, Attrs, E)
  end, Events),
  {ok, Events1}.



filtermap_event(Types, AttrList, #event{key = {_,_,_,_,Type}, attrs = Attrs1} = E) ->
  case lists:member(Type, Types) of
    false -> false;
    _ ->
      Attrs2 = lists:filter(fun ({Key, _}) ->
        lists:member(Key, AttrList)
      end, Attrs1),
      {true, record_to_event_delta_map(E#event{attrs = Attrs2})}
  end.
  



select_plug_request_info(#{
  pid := Pid, instance_id := InstanceId,
  started_at := StartedAt, stopped_at := StoppedAt
}) ->
  Match = ets:fun2ms(fun
    (#event{key = {InstanceId1, AtSec, AtMcs, _, _}, pid1 = Pid1, _ = '_'} = E)
    when InstanceId1 =:= InstanceId
    andalso Pid1 =:= Pid
    andalso (AtSec*1000*1000 + AtMcs) >= StartedAt
    andalso (AtSec*1000*1000 + AtMcs) =< StoppedAt ->
      E
  end),

  Attrs = batiscaph_delta_plug:desired_request_attrs(),
  Types = batiscaph_delta_plug:desired_request_types(),

  Events = mnesia:dirty_select(event, Match),
  Events1 = lists:filtermap(fun (E) ->
    filtermap_event(Types, Attrs, E)
  end, Events),

  Info = batiscaph_delta_plug:produce_request_info(Events1, #{<<"Pid">> => Pid}),

  {ok, Info}.



select_cowboy_request_info(#{
  pid := Pid, instance_id := InstanceId,
  started_at := StartedAt, stopped_at := StoppedAt
}) ->
  Match = ets:fun2ms(fun
    (#event{key = {InstanceId1, AtSec, AtMcs, _, _}, pid1 = Pid1, _ = '_'} = E)
    when InstanceId1 =:= InstanceId
    andalso Pid1 =:= Pid
    andalso (AtSec*1000*1000 + AtMcs) >= StartedAt
    andalso (AtSec*1000*1000 + AtMcs) =< StoppedAt ->
      E
  end),

  Attrs = batiscaph_delta_cowboy:desired_request_attrs(),
  Types = batiscaph_delta_cowboy:desired_request_types(),

  Events = mnesia:dirty_select(event, Match),
  Events1 = lists:filtermap(fun (E) ->
    filtermap_event(Types, Attrs, E)
  end, Events),

  Info = batiscaph_delta_cowboy:produce_request_info(Events1, #{<<"Pid">> => Pid}),

  {ok, Info}.
