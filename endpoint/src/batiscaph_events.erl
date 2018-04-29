-module(batiscaph_events).
-export([create_tables/0, drop_tables/0]).
-export([insert/1]).

% different select queries
-export([
  select_instances_infos_with_ids/1,
  select_events/1,
  select_plug_request_info/1, select_cowboy_request_info/1,
  select_instances_ids/0
]).
-export([event/3, transform/2]).



-callback create_tables() -> ok.
-callback drop_tables() -> ok.
-callback insert([map()]) -> ok.

-callback select_instances_infos_with_ids([binary()])
  -> {ok, [map()]}.

-callback select_events(#{
  instance_id := binary(), types := [binary()], attrs := [binary()],
  earlier_than := now, limit := non_neg_integer()
}) ->
  {ok, [map()]}.

-callback select_plug_request_info(#{
  pid := binary(), instance_id := binary(),
  started_at := non_neg_integer(), stopped_at := non_neg_integer()
}) ->
  {ok, map()}.

-callback select_cowboy_request_info(#{
  pid := binary(), instance_id := binary(),
  started_at := non_neg_integer(), stopped_at := non_neg_integer()
}) ->
  {ok, map()}.

-callback select_instances_ids() -> {ok, [binary()]}.



%
%
%



create_tables() ->
  batiscaph_events_clickhouse:create_tables().

drop_tables() ->
  batiscaph_events_clickhouse:drop_tables().

insert(Events) ->
  batiscaph_events_clickhouse:insert(Events).

select_instances_infos_with_ids(Ids) ->
  batiscaph_events_clickhouse:select_instances_infos_with_ids(Ids).

select_events(Opts) ->
  batiscaph_events_clickhouse:select_events(Opts).

select_plug_request_info(Opts) ->
  batiscaph_events_clickhouse:select_plug_request_info(Opts).

select_cowboy_request_info(Opts) ->
  batiscaph_events_clickhouse:select_cowboy_request_info(Opts).

select_instances_ids() ->
  batiscaph_events_clickhouse:select_instances_ids().



%
%
%



event(InstanceId, now, Type) ->
  % TODO: server time might differ from client time
  % need to align those timings, detect time difference
  % when connecting with client
  Microseconds = erlang:system_time(micro_seconds),
  event(InstanceId, Microseconds, Type);

event(InstanceId, Microseconds, Type) when is_integer(Microseconds) ->
  #{
    <<"InstanceId">> => InstanceId,
    <<"AtSec">> => (Microseconds div (1000*1000)),
    <<"AtMcs">> => (Microseconds rem (1000*1000)),
    <<"SubId">> => gen_sub_id(),
    <<"Type">> => Type
  }.



% these keys never get into attrs
special_keys() ->
  [at, type, pid1, pid2].

transform([], _Opts) -> [];
transform([#{type := <<"p1 ", _/binary>>, pid1 := Pid1} = E0 | Rest], Opts) ->
  % typespec specific transform
  E1 = #{<<"Pid1">> => Pid1},
  [transform0(E1, E0, Opts) | transform(Rest, Opts)];

transform([#{type := <<"p2 ", _/binary>>, pid1 := Pid1, pid2 := Pid2} = E0 | Rest], Opts) ->
  % typespec specific transform
  E1 = #{<<"Pid1">> => Pid1, <<"Pid2">> => Pid2},
  [transform0(E1, E0, Opts) | transform(Rest, Opts)].



transform0(E1, E0, #{instance_id := InstanceId}) ->
  #{at := At0, type := Type} = E0,
  {AtSec, AtMcs} = at_value(At0),

  Attrs0 = maps:to_list(maps:without(special_keys(), E0)),
  Attrs1 = [{to_binary(K), to_binary(V)} || {K, V} <- Attrs0],
  {Keys, Values} = lists:unzip(Attrs1),

  E1#{
    <<"InstanceId">> => InstanceId,
    <<"AtSec">> => AtSec, <<"AtMcs">> => AtMcs,
    <<"Type">> => Type,
    <<"SubId">> => gen_sub_id(),
    <<"Attrs.Key">> => Keys,
    <<"Attrs.Value">> => Values
  }.



% value that trace_ts messages provide us
at_value({Mega, Sec, Micro}) ->
  {Mega*1000*1000 + Sec, Micro};

% the value that we get from erlang:system_time(micro_seconds)
at_value(FullMcs) when is_integer(FullMcs) ->
  AtScs = FullMcs div (1000*1000),
  AtMcs = FullMcs rem (1000*1000),
  {AtScs, AtMcs}.

to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value,latin1);
to_binary(Value) when is_binary(Value) -> Value.



% sub_id is an additional id that added to
% be able to distinguish two events happened at exactly same time
% currently just generated randomly
% should be okay for now
%
% more correct approach would probably be just enumerate events
% but it requires buffering and sorting -- random is okay for now
%
% SubId has type UInt16 in table
% generate id that fits into UInt16
gen_sub_id() ->
  rand:uniform(65535).
