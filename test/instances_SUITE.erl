-module(instances_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  instances_running_and_stopped/1
]).



% Tests in this suite check how information about instances
% changes on service.



all() ->
  [instances_running_and_stopped].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  Config.

end_per_suite(Config) ->
  Config.



instances_running_and_stopped(Config) ->
  {ok, UserId, AccessKey} = vt_web:create_user_and_return_access_key(),

  % no instance started for this user
  {ok, []} = vt:api_request(get, instances, #{user_id => UserId}),

  % subscribe for communication
  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey
  }),

  {summary_info, #{instance_id := <<InstanceId/binary>>}} = vt_endpoint:received_from_probe(summary_info),
  {response, _, apply_config, ok} = vt_endpoint:received_from_probe(apply_config),

  % instance connected and sent info
  % should appear as 'connected'
  {ok, [Instance1]} = vt:api_request(get, instances, #{user_id => UserId}),
  #{<<"InstanceId">> := InstanceId, <<"StartedAt">> := _, <<"Connected">> := true} = Instance1,

  ok = vt:stop_docker_container(AppContainer),

  % TODO: find a way to ensure that container stopped
  % and tcp connection is closed
  timer:sleep(1000),

  % instance is stopped
  {ok, [Instance2]} = vt:api_request(get, instances, #{user_id => UserId}),
  #{<<"InstanceId">> := InstanceId, <<"StartedAt">> := At1, <<"StoppedAt">> := At2, <<"Connected">> := false} = Instance2,
  true = At1 < At2,

  ok.
