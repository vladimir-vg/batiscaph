-module(vision_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([read_config/0]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->

  % start vision probe only is it's provided as dependency
  try vision_probe_app:module_info() of
  [_ | _] -> application:ensure_all_started(vision_probe)
  catch error:undef -> ok
  end,

  [_ | _] = vision_probe_protocol:mention_used_atoms(),
  ok = read_config(),
  vision_web:start_cowboy(),

  % for now save it as env variable
  % later probably we need to move it into ETS
  % or even generate module at runtime, as Tristan suggested
  {ok, PgQueries} = eql:compile(code:priv_dir(vision) ++ "/pg_queries.sql"),
  {ok, ClkQueries} = eql:compile(code:priv_dir(vision) ++ "/clk_queries.sql"),
  ok = application:set_env(vision, pg_queries, PgQueries),
  ok = application:set_env(vision, clk_queries, ClkQueries),

  % useful to cache babel output
  % web_page_cache = ets:new(web_page_cache, [public, named_table, set]),
  test_subscriptions = ets:new(test_subscriptions, [public, named_table, bag]),

  vision_sup:start_link().



read_config() ->
  application:set_env(vision, http_port, list_to_integer(os:getenv("VISION_ENDPOINT_HTTP_PORT"))),
  application:set_env(vision, clickhouse_dbname, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_DB"))),
  application:set_env(vision, clickhouse_url, list_to_binary(os:getenv("VISION_ENDPOINT_CLICKHOUSE_URL"))),
  % application:set_env(vision, neo4j_url, list_to_binary(os:getenv("VISION_ENDPOINT_NEO4J_HTTP_URL"))),

  Url = os:getenv("VISION_ENDPOINT_POSTGRES_URL"),
  {ok, {postgres, Auth, Host, _Port, "/"++DbName, _}} = http_uri:parse(Url, [{scheme_defaults, [{postgres, 5432}]}]),
  [Login, Password] = string:split(Auth, ":"),

  ok = application:set_env(epgpool, database_host, Host),
  ok = application:set_env(epgpool, database_name, DbName),
  ok = application:set_env(epgpool, database_user, Login),
  ok = application:set_env(epgpool, database_password, Password),
  ok = application:set_env(epgpool, log_errors_verbose, true),

  ok.

