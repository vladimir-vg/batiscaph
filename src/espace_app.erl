-module(espace_app).
-behaviour(application).
-export([start/2, stop/1]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->
  case node() of
    'nonode@nohost' ->
      io:format("Erlang node started in non distributed mode, name must be specified\n"),
      ok;
    _ ->
      ok = read_config(),
      es_web:restart_cowboy(),
      ok
  end,
  espace_sup:start_link().



% restart() ->
%   es_web:restart_cowboy(),
%   ok.

read_config() ->
  DBName = list_to_binary(os:getenv("CLICKHOUSE_DB", "espace")),
  DBUrl = list_to_binary(os:getenv("CLICKHOUSE_URL", "http://0.0.0.0:8123/")),
  NeoUrl = list_to_binary(os:getenv("NEO4J_HTTP_URL", "http://neo4j:neo4j@0.0.0.0:7474/")),
  application:set_env(espace, clickhouse_dbname, DBName),
  application:set_env(espace, clickhouse_url, DBUrl),
  application:set_env(espace, neo4j_url, NeoUrl),
  ok.

