-module(delta_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  spawn_process/1
]).



% Tests in this suite call different code on client
% receive delta, and check it's correctness.



all() ->
  [
    spawn_process
  ].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  % create user
  {ok, UserId, AccessKey} = vt_web:create_user_and_return_access_key(),

  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),

  Port = 8084,
  {ok, AppContainer} = start_webapp(PrivDir, AccessKey, Port),

  InstanceId =
    receive {probe_connected, <<Id/binary>>} -> Id
    after 5000 -> error(probe_connection_timeout)
    end,

  % wait until tracing is fully enabled
  {response, _, apply_config, ok} = vt_endpoint:received_from_probe(apply_config),

  BaseUrl = vt:base_url(AppContainer, Port),
  [{app_base_url, BaseUrl}, {app_user_id, UserId}, {app_instance_id, InstanceId} | Config].

end_per_suite(Config) ->
  Config.



start_webapp(PrivDir, AccessKey, Port) ->
  {ok, AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"HTTP_PORT">> => Port,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey,
    wait_for_application => erlang_app1
  }),
  {ok, AppContainer}.



%%%%%%%



% for now a cowboy request is the only convenient way to start tracing on process
% that's why these test are written through http requests
% later this interfacing part might be rewritten in some other manner



spawn_process(Config) ->
  InstanceId = proplists:get_value(app_instance_id, Config),
  UserId = proplists:get_value(app_user_id, Config),

  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),
  BaseUrl = proplists:get_value(app_base_url, Config),

  {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, <<BaseUrl/binary, "/spawn_process">>, [], <<>>, []),
  ok = hackney:skip_body(ClientRef),

  {events, _} = vt_endpoint:received_from_probe(events),

  % open websocket and subscribe to instance delta
  % as browser client would do
  {ok, Ws} = vt_endpoint:ws_connect(),
  ok = vt_endpoint:ws_send(Ws, subscribe_to_instance, InstanceId),

  #{<<"cowboy-requests">> := Reqs, <<"erlang-processes">> := Procs} = vt_endpoint:ws_delivered(Ws, delta),
  [ReqPid] = [P || {_, #{<<"Path">> := <<"/spawn_process">>, <<"Pid">> := P}} <- maps:to_list(Reqs)],
  Proc = maps:get(ReqPid, Procs),
  #{} = Proc,

  % child process should exist
  [_] = [P || {_, #{<<"ParentPid">> := Pid1} = P} <- maps:to_list(Procs), Pid1 =:= ReqPid],

  ok.



