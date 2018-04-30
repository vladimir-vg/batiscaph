-module(batiscaph_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([read_config/0]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->

  % start batiscaph probe only is it's provided as dependency
  try batiscaph_probe_app:module_info() of
  [_ | _] -> application:ensure_all_started(batiscaph_probe)
  catch error:undef -> ok
  end,

  [_ | _] = batiscaph_probe_protocol:mention_used_atoms(),
  ok = read_config(),
  batiscaph_web:start_cowboy(),

  % for now save it as env variable
  % later probably we need to move it into ETS
  % or even generate module at runtime, as Tristan suggested
  % {ok, PgQueries} = eql:compile(code:priv_dir(batiscaph) ++ "/pg_queries.sql"),
  {ok, ClkQueries} = eql:compile(code:priv_dir(batiscaph) ++ "/clk_queries.sql"),
  % ok = application:set_env(batiscaph, pg_queries, PgQueries),
  ok = application:set_env(batiscaph, clk_queries, ClkQueries),

  % useful to cache babel output
  % web_page_cache = ets:new(web_page_cache, [public, named_table, set]),
  test_subscriptions = ets:new(test_subscriptions, [public, named_table, bag]),
  delta_subscribers = ets:new(delta_subscribers, [public, named_table, set, {keypos, 2}]),

  batiscaph_sup:start_link().



read_config() ->
  read_env_and_update("BATISCAPH_ENDPOINT_HTTP_PORT", http_port, fun erlang:list_to_integer/1),
  read_env_and_update("BATISCAPH_ENDPOINT_CLICKHOUSE_DB", clickhouse_dbname, fun erlang:list_to_binary/1),
  read_env_and_update("BATISCAPH_ENDPOINT_CLICKHOUSE_URL", clickhouse_url, fun erlang:list_to_binary/1),

  case {application:get_env(batiscaph, clickhouse_dbname), application:get_env(batiscaph, clickhouse_url)} of
    {{ok, _}, {ok, _}} ->
      lager:info("Using Clickhouse for storage"),
      application:set_env(batiscaph, storage_type, clickhouse),
      ok;
    _ ->
      % clickhouse env vars are not defined
      % use mnesia instead
      lager:info("Using Mnesia for storage, because no Clickhouse config env was provided"),
      application:set_env(batiscaph, storage_type, mnesia),
      ok = batiscaph_events_mnesia:setup(),
      ok
  end,

  % application:set_env(batiscaph, neo4j_url, list_to_binary(os:getenv("BATISCAPH_ENDPOINT_NEO4J_HTTP_URL"))),

  % Url = os:getenv("BATISCAPH_ENDPOINT_POSTGRES_URL"),
  % {ok, {postgres, Auth, Host, _Port, "/"++DbName, _}} = http_uri:parse(Url, [{scheme_defaults, [{postgres, 5432}]}]),
  % [Login, Password] = string:split(Auth, ":"),
  % 
  % ok = application:set_env(epgpool, database_host, Host),
  % ok = application:set_env(epgpool, database_name, DbName),
  % ok = application:set_env(epgpool, database_user, Login),
  % ok = application:set_env(epgpool, database_password, Password),
  % ok = application:set_env(epgpool, log_errors_verbose, true),

  ok.

read_env_and_update(EnvKey, AppKey, ReadFun) ->
  case os:getenv(EnvKey) of
    false -> ok;
    "" -> ok;
    Value when is_list(Value) ->
      application:set_env(batiscaph, AppKey, ReadFun(Value)),
      ok
  end.