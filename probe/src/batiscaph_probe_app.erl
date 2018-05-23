-module(batiscaph_probe_app).
-behaviour(application).
-export([start/2, stop/1]).



stop(_State) ->
  ok.



start(_StartType, _StartArgs) ->
  [_ | _] = batiscaph_probe_session:mention_used_atoms(),

  batiscaph_probe_procs = ets:new(batiscaph_probe_procs, [public, set, named_table]),

  ok = read_config(),
  batiscaph_probe_sup:start_link().



% get application environment in order
% OS env variables take precedence
read_config() ->
  ok = read_env_and_update("BATISCAPH_PROBE_ENDPOINT_URL", endpoint_url, fun erlang:list_to_binary/1),
  % ok = read_env_and_update("BATISCAPH_PROBE_ACCESS_KEY", access_key, fun erlang:list_to_binary/1),

  case application:get_env(batiscaph_probe, endpoint_url) of
    undefined ->
      % if no env variable provided, then we assume that server is running on same machine
      % and uses default port
      application:set_env(batiscaph_probe, endpoint_url, <<"http://127.0.0.1:8099/probe">>),
      ok;

    {ok, EndpointUrl} when is_binary(EndpointUrl) -> ok
  end,
  ok.

read_env_and_update(EnvKey, AppKey, ReadFun) ->
  case os:getenv(EnvKey) of
    false -> ok;
    "" -> ok;
    Value when is_list(Value) ->
      application:set_env(batiscaph_probe, AppKey, ReadFun(Value)),
      ok
  end.

