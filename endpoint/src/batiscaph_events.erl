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
