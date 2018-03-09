-module(vision_clk_events).
-export([columns/0, create_tables/0, drop_tables/0]).
-export([insert/1, event/3]).

% different select queries
-export([
  select_instances_infos_with_ids/1
]).


columns() ->
  [
    {<<"AtSec">>, <<"DateTime">>},
    {<<"AtMcs">>, <<"UInt32">>},
    % id of event for given AtSec, AtMcs and InstanceId
    % need to distinguish several events that occured at exactly same time (AtSec,AtMcs)
    {<<"SubId">>, <<"UInt16">>},
    {<<"InstanceId">>, <<"String">>},
    {<<"Type">>, <<"String">>},
    {<<"Pid1">>, <<"String">>},
    {<<"Pid2">>, <<"String">>},
    {<<"Fields">>, {nested, [
      {<<"Key">>, <<"String">>},
      {<<"Value">>, <<"String">>}
    ]}}
  ].



create_tables() ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  SQL = iolist_to_binary([
    "CREATE TABLE `", DBName, "`.events (\n",
    "\tAtSec DateTime,\n",
    "\tAtMcs UInt32,\n",
    % might be better to turn it into FixedString(32)
    "\tInstanceId String,\n",
    "\tType String,\n",
    "\tPid1 String,\n",
    "\tPid2 String,\n",

    % into this nested column fall all custom fields
    "\tFields Nested (\n",
    "\t\tKey String,\n",
    "\t\tValue String\n",
    "\t),\n",
    "\tEventDate Date DEFAULT toDate(AtSec)\n",
    ") ENGINE=MergeTree(EventDate, (InstanceId, (AtSec, AtMcs)), 8192)\n"
  ]),
  ok = clickhouse:execute(SQL),
  ok.



drop_tables() ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  SQL = iolist_to_binary([
    "DROP TABLE `", DBName, "`.events\n"
  ]),
  clickhouse:execute(SQL),
  ok.



insert(Events) ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  InsertedColumns = [<<"InstanceId">>, <<"AtSec">>, <<"AtMcs">>, <<"Type">>],
  Columns = [C || {K, _} = C <- columns(), lists:member(K, InsertedColumns)],
  ok = clickhouse:insert(DBName, <<"events">>, Columns, Events),
  ok.

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
    <<"Type">> => Type
  }.



select_instances_infos_with_ids([]) -> {ok, []};
select_instances_infos_with_ids(Ids) ->
  {ok, Q} = application:get_env(vision, clk_queries),
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  {ok, SQL} = eql:get_query(select_instances_infos_with_ids, Q, [{dbname, DBName}, {ids, clickhouse:list_sql(Ids)}]),
  {ok, Body} = clickhouse:execute(SQL),
  {ok, Events} = clickhouse:parse_rows(maps, Body),

  CurrentlyConnected = [Id || {Id, _Attrs} <- gen_tracker:list(probes, [])],

  Instances = lists:foldl(fun
    (#{<<"Type">> := <<"vision 0 connection-start">>, <<"InstanceId">> := Id, <<"At">> := At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Map1 = Map#{<<"Connected">> => lists:member(Id, CurrentlyConnected)},
      Acc#{Id => maps:put(<<"StartedAt">>, At, Map1)};

    (#{<<"Type">> := <<"vision 0 connection-stop">>, <<"InstanceId">> := Id, <<"At">> := At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Acc#{Id => maps:put(<<"StoppedAt">>, At, Map)}
  end, #{}, Events),

  {ok, maps:values(Instances)}.
