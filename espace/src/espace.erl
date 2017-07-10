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
  ok = es_events:create_table(),
  ok.

drop_tables() ->
  ok = es_events:drop_table(),
  ok.