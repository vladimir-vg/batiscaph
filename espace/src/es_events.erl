-module(es_events).
-export([create_table/0, drop_table/0, store/2]).



columns() ->
  [
    {<<"at">>, <<"DateTime">>},
    {<<"at_mcs">>, <<"UInt32">>},
    {<<"instance_id">>, <<"String">>},
    {<<"pid">>, <<"String">>},
    {<<"type">>, <<"String">>}
  ].

create_table() ->
  DBName = espace:get_prop(clickhouse_dbname),
  Columns = iolist_to_binary([["\t", Name, " ", Type, ",\n"] || {Name, Type} <- columns()]),
  SQL = iolist_to_binary([
    "CREATE TABLE `", DBName, "`.events (\n",
    Columns,
    "\tevent_date Date DEFAULT toDate(at)\n",
    ") ENGINE=MergeTree(event_date, (instance_id, type, pid, at), 8192)\n"
  ]),
  ok = clickhouse:execute(SQL),
  ok.

drop_table() ->
  DBName = espace:get_prop(clickhouse_dbname),
  SQL = iolist_to_binary(["DROP TABLE `", DBName, "`.events\n"]),
  ok = clickhouse:execute(SQL),
  ok.



store(InstanceId, Events) ->
  DBName = espace:get_prop(clickhouse_dbname),
  ok = clickhouse:insert(DBName, <<"events">>, columns(), Events, fun (Item) ->
    Item#{<<"instance_id">> => InstanceId}
  end),
  ok.
