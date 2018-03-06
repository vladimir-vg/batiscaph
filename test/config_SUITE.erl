-module(config_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  authenticate_and_ask_for_config/1
]).



all() ->
  [authenticate_and_ask_for_config].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  Config.

end_per_suite(Config) ->
  Config.



authenticate_and_ask_for_config(Config) ->
  EndpointNode = vt:endpoint_node(),
  WebappNode = vt:webapp_node(),
  {ok, UserId, AccessKey} = rpc:call(WebappNode, 'Elixir.Vision.Test', create_user_and_return_access_key, []),

  % subscribe for communication
  ok = rpc:call(EndpointNode, vision_test, subscribe_to_session, [self(), #{user_id => UserId}]),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, _AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey
  }),

  {summary_info, #{
    probe_version := <<"0.1.0">>,
    dependency_in := [{<<"erlang_app1">>, <<"1.2.3-test1">>}],
    instance_id := <<InstanceId/binary>>
  }} = vt:received_from_probe(summary_info),

  {request, ReqId1, get_user_config, _} = vt:sent_to_probe(get_user_config),
  {response, ReqId1, get_user_config, #{}} = vt:received_from_probe(get_user_config),

  {request, ReqId2, apply_config, #{}} = vt:sent_to_probe(apply_config),
  {response, ReqId2, apply_config, ok} = vt:received_from_probe(apply_config),

  {ok, [#{<<"instanceId">> := InstanceId}]} = vt:api_request(get, instances, [{user_id, UserId}]),

  ok.
