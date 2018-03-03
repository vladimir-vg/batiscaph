-module(vision_app).
-behaviour(application).
-export([start/2, stop/1]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->
  [_ | _] = vision_probe_protocol:mention_used_atoms(),
  ok = read_config(),
  vision_web:start_cowboy(),

  % useful to cache babel output
  web_page_cache = ets:new(web_page_cache, [public, named_table, set]),
  test_subscriptions = ets:new(test_subscriptions, [public, named_table, bag]),

  vision_sup:start_link().



read_config() ->
  application:set_env(vision, http_port, list_to_integer(os:getenv("VISION_ENDPOINT_HTTP_PORT"))),
  application:set_env(vision, clickhouse_dbname, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_DB"))),
  application:set_env(vision, clickhouse_url, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_URL"))),
  application:set_env(vision, neo4j_url, list_to_binary(os:getenv("VISION_ENDPOINT_NEO4J_HTTP_URL"))),

  ok.

