-module(batiscaph_app).
-behaviour(application).
-export([start/2, stop/1]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->
  [_ | _] = vision_probe_protocol:mention_used_atoms(),

  case node() of
    'nonode@nohost' ->
      io:format("Erlang node started in non distributed mode, name must be specified\n"),
      ok;
    _ ->
      ok = read_config(),
      case application:get_env(batiscaph, http_port) of
        {ok, _} -> batiscaph_web:restart_cowboy();
        _ -> ok
      end,
      ok
  end,

  % useful to cache babel output
  web_page_cache = ets:new(web_page_cache, [public, named_table, set]),
  test_subscriptions = ets:new(test_subscriptions, [public, named_table, bag]),

  batiscaph_sup:start_link().



read_config() ->
  application:set_env(batiscaph, http_port, list_to_integer(os:getenv("VISION_ENDPOINT_HTTP_PORT"))),
  application:set_env(batiscaph, clickhouse_dbname, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_DB"))),
  application:set_env(batiscaph, clickhouse_url, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_URL"))),
  application:set_env(batiscaph, neo4j_url, list_to_binary(os:getenv("VISION_ENDPOINT_NEO4J_HTTP_URL"))),

  ok.

