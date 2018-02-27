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
  EndpointNode = vt:endpoint_node(),
  WebappNode = vt:webapp_node(),
  {ok, UserId, AccessKey} = rpc:call(WebappNode, 'Elixir.Vision.Test', create_user_and_return_access_key, []),

  % subscribe for communication
  ok = rpc:call(EndpointNode, vision_test, subscribe_to_first_session, [self(), #{user_id => UserId}]),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, _AppContainer} = vt:start_docker_container(<<"authorized_app_run">>, <<"vision-test/example_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey
  }),

  {summary_info, #{
    probe_version := <<"0.1.0">>,
    dependency_in := [{<<"example_app1">>, <<"1.2.3-test1">>}],
    instance_id := <<_/binary>>
  }} = vt:received_from_probe(),

  {request, ReqId, get_config, #{}} = vt:received_from_probe(),
  {response, ReqId, get_config, #{}} = vt:sent_to_probe(),

  ok.
