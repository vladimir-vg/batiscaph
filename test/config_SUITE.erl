-module(config_SUITE).
-export([
  all/0, init_per_suite/1, end_per_suite/1,
  authenticate_and_ask_for_config/1
]).



all() ->
  [authenticate_and_ask_for_config].



init_per_suite(Config) ->
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_fresh_endpoint_running(#{logdir => PrivDir}),
  ok = vt:ensure_fresh_webapp_running(#{logdir => PrivDir}),
  Config.

end_per_suite(Config) ->
  Config.



authenticate_and_ask_for_config(Config) ->
  % PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  % {ok, _AppContainer} = vt:start_docker_container(<<"authorized_app_run">>, <<"vision-test/example_app1:latest">>, #{
  %   host_network => true, logdir => PrivDir,
  %   <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
  %   <<"VISION_PROBE_ACCESS_KEY">> => <<"1">>
  % }),

  ok.
