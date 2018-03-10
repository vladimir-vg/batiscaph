-module(http_requests_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([receive_http_request_event/1]).



all() ->
  [receive_http_request_event].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  % create user
  WebappNode = vt:webapp_node(),
  {ok, UserId, AccessKey} = rpc:call(WebappNode, 'Elixir.Vision.Test', create_user_and_return_access_key, []),

  % subscribe to events of upcoming app run
  ok = rpc:call(vt:endpoint_node(), vision_test, subscribe_to_session, [self(), #{user_id => UserId}]),

  Port = 8083,
  {ok, AppContainer} = start_webapp(PrivDir, AccessKey, Port),

  {summary_info, #{instance_id := <<InstanceId/binary>>}} = vt:received_from_probe(summary_info),

  % wait until tracing is fully enabled
  {response, _, apply_config, ok} = vt:received_from_probe(apply_config),

  BaseUrl = vt:base_url(AppContainer, Port),
  [{app_base_url, BaseUrl}, {app_user_id, UserId}, {app_instance_id, InstanceId} | Config].

end_per_suite(Config) ->
  Config.



start_webapp(PrivDir, AccessKey, Port) ->
  {ok, AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/phoenix_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"DATABASE_URL">> => <<"postgres://postgres:postgres@127.0.0.1/vision_test_phoenix_app1">>,
    <<"HTTP_PORT">> => Port,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey,
    runtype => phoenix,
    wait_for_application => phoenix_app1
  }),
  {ok, AppContainer}.



receive_http_request_event(Config) ->
  InstanceId = proplists:get_value(app_instance_id, Config),
  UserId = proplists:get_value(app_user_id, Config),
  EndpointNode = vt:endpoint_node(),

  ok = rpc:call(EndpointNode, vision_test, subscribe_to_session, [self(), #{user_id => UserId}]),
  BaseUrl = proplists:get_value(app_base_url, Config),

  % open websocket and subscribe to instance delta
  % as browser client would do
  {ok, Pid} = vt_ws:connect_to_endpoint(),
  ok = vt_ws:subscribe_to_instance(Pid, InstanceId),
  ok = vt_ws:skip_all_messages(Pid),

  {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, BaseUrl, [], <<>>, []),
  ok = hackney:skip_body(ClientRef),

  {events, Events} = vt:received_from_probe(events),
  Events1 = lists:sort(fun (A, B) -> maps:get(at, A) < maps:get(at, B) end, Events),
  [<<"p1 plug:request start">>, <<"p1 plug:request stop">>] = [T || #{type := T} <- Events1],

  #{<<"plug:requests">> := #{} = Reqs} = vt_ws:receive(delta, Pid),

  % key of the request is unknown
  [{_, Req1}] = maps:to_list(Reqs),
  #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"Pid1">> := _} = Req1,

  ok.
