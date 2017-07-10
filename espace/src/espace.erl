-module(espace).
-export([start/0, restart/0, get_prop/1, create_tables/0, drop_tables/0]).



start() ->
  ok = read_config(),
  application:ensure_all_started(espace),
  es_web:restart_cowboy(),
  ok.

restart() ->
  es_web:restart_cowboy(),
  ok.

read_config() ->
  DBName = list_to_binary(os:getenv("CLICKHOUSE_DB", "espace")),
  DBUrl = list_to_binary(os:getenv("CLICKHOUSE_URL", "http://0.0.0.0:8123/")),
  application:set_env(espace, clickhouse_dbname, DBName),
  application:set_env(espace, clickhouse_url, DBUrl),
  ok.

get_prop(Name) ->
  case erlang:get({app_config_prop, Name}) of
    undefined ->
      {ok, Value} = application:get_env(espace, Name),
      put({app_config_prop, Name}, Value),
      Value;
    Value -> Value
  end.



create_tables() ->
  DBName = get_prop(clickhouse_dbname),
  SQL = iolist_to_binary([
    "CREATE TABLE `", DBName, "`.events (\n",
    "\tevent_date Date DEFAULT toDate(reported_at),\n",
    "\tinstance_id String,\n",
    "\tevent_type String,\n",
    "\tpid String,\n",
    "\treported_at DateTime\n",
    ") ENGINE=MergeTree(event_date, (instance_id, event_type, pid, reported_at), 8192)\n"
  ]),
  ok = clickhouse:execute(SQL),
  ok.

drop_tables() ->
  DBName = get_prop(clickhouse_dbname),
  SQL = iolist_to_binary(["DROP TABLE `", DBName, "`.events\n"]),
  ok = clickhouse:execute(SQL),
  ok.