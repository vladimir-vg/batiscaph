-module(delta_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  spawn_process/1,
  subscribe_to_process_info/1
]).



% Tests in this suite call different code on client
% receive delta, and check it's correctness.



all() ->
  [
    spawn_process,
    subscribe_to_process_info
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

  #{<<"erlang-processes">> := Procs} = Delta = vt_endpoint:ws_delivered(Ws, delta),
  {ok, ReqPid, _From, _To} = pid_for_request_path(<<"/spawn_process">>, Delta),
  #{} = maps:get(ReqPid, Procs),

  % child process should exist
  [_] = [P || {_, #{<<"ParentPid">> := Pid1} = P} <- maps:to_list(Procs), Pid1 =:= ReqPid],

  ok.



subscribe_to_process_info(Config) ->
  % spawn two processes, receive delta
  % subscribe to process_info of first process
  % receive detailed updates for it
  % unsubscribe, subscribe to other
  % receive detailed updates for other

  InstanceId = proplists:get_value(app_instance_id, Config),
  UserId = proplists:get_value(app_user_id, Config),

  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),
  BaseUrl = proplists:get_value(app_base_url, Config),

  {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, <<BaseUrl/binary, "/subscribe_to_process_info">>, [], <<>>, []),
  ok = hackney:skip_body(ClientRef),

  {events, _} = vt_endpoint:received_from_probe(events),
  {ok, Ws} = vt_endpoint:ws_connect(),
  ok = vt_endpoint:ws_send(Ws, subscribe_to_instance, InstanceId),

  #{<<"erlang-processes">> := Procs} = Delta1 = vt_endpoint:ws_delivered(Ws, delta),
  {ok, ReqPid, From, _To} = pid_for_request_path(<<"/subscribe_to_process_info">>, Delta1),
  #{} = maps:get(ReqPid, Procs),

  % two children should be spawned
  [ChildPid1, ChildPid2] = [P || {_, #{<<"ParentPid">> := Pid1, <<"SpawnedAt">> := At1} = P} <- maps:to_list(Procs), Pid1 =:= ReqPid, At1 > From],

  % no detailed info in original delta
  0 = maps:size(maps:get(<<"erlang-processes-info">>, Delta1, #{})),

  ok = vt_endpoint:ws_send(Ws, subscribe_to_process_info, #{instance_id => InstanceId, pid => ChildPid1}),

  % expect to receive detailed info for one of the children
  #{<<"erlang-processes-info">> := #{ChildPid1 := Details1}} = Delta2 = vt_endpoint:ws_delivered(Ws, delta),
  1 = maps:size(maps:get(<<"erlang-processes-info">>, Delta2, #{})),

  % only one entry in changes
  #{<<"Pid">> := ChildPid1, <<"Changes">> := Changes1} = Details1,
  [{_, #{<<"trap_exit">> := false, <<"message_queue_len">> := 0}}] = maps:to_list(Changes1),

  % subscribe to second child
  ok = vt_endpoint:ws_send(Ws, unsubscribe_from_process_info, #{instance_id => InstanceId, pid => ChildPid1}),
  ok = vt_endpoint:ws_send(Ws, subscribe_to_process_info, #{instance_id => InstanceId, pid => ChildPid2}),

  % expect to receive detailed info for one of the children
  #{<<"erlang-processes-info">> := #{ChildPid2 := Details2}} = Delta3 = vt_endpoint:ws_delivered(Ws, delta),
  1 = maps:size(maps:get(<<"erlang-processes-info">>, Delta3, #{})),

  % only one entry in changes
  #{<<"Pid">> := ChildPid2, <<"Changes">> := Changes2} = Details2,
  [{_, #{<<"trap_exit">> := false, <<"message_queue_len">> := 0}}] = maps:to_list(Changes2),

  ok.



pid_for_request_path(Path, Delta) ->
  #{<<"cowboy-requests">> := Reqs} = Delta,
  [Req] = [R || {_, #{<<"Path">> := Path1, <<"Pid">> := _} = R} <- maps:to_list(Reqs), Path =:= Path1],
  #{<<"Pid">> := Pid, <<"init">> := #{<<"StartedAt">> := From}, <<"handle">> := #{<<"StoppedAt">> := To}} = Req,
  {ok, Pid, From, To}.
