-module(vision_clk_events).
-export([columns/0, create_tables/0, drop_tables/0]).



columns() ->
  [
    {<<"AtSec">>, <<"DateTime">>},
    {<<"AtMcs">>, <<"UInt32">>},
    % id of event for given AtSec, AtMcs and InstanceId
    % need to distinguish several events that occured at exactly same time (AtSec,AtMcs)
    {<<"SubId">>, <<"UInt16">>},
    {<<"InstanceId">>, <<"String">>},
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
