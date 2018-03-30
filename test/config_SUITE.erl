-module(config_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  authenticate_and_ask_for_config/1
]).



% Tests in this suite check correct behaviour of config
% transmission and application



all() ->
  [authenticate_and_ask_for_config].



init_per_suite(Config) ->
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  Config.

end_per_suite(Config) ->
  Config.



authenticate_and_ask_for_config(Config) ->
  {ok, UserId, AccessKey} = vt_web:create_user_and_return_access_key(),

  % subscribe for communication
  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, _AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey
  }),

  receive {probe_connected, _InstanceId} -> ok
  after 5000 -> error(probe_connection_timeout)
  end,

  {request, ReqId1, get_user_config, _} = vt_endpoint:sent_to_probe(get_user_config),
  {response, ReqId1, get_user_config, #{}} = vt_endpoint:received_from_probe(get_user_config),

  {request, ReqId2, apply_config, #{}} = vt_endpoint:sent_to_probe(apply_config),
  {response, ReqId2, apply_config, ok} = vt_endpoint:received_from_probe(apply_config),

  {summary_info, #{
    probe_version := <<"0.1.0">>,
    dependency_in := [{<<"erlang_app1">>, <<"1.2.3-test1">>}]
  }} = vt_endpoint:received_from_probe(summary_info),

  ok.
