-module(http_requests_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  receive_http_request_event_and_delta/1
]).



all() ->
  [
    receive_http_request_event_and_delta
  ].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  % create user
  {ok, UserId, AccessKey} = vt_web:create_user_and_return_access_key(),

  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),

  Port = 8083,
  {ok, AppContainer} = start_webapp(PrivDir, AccessKey, Port),

  {summary_info, #{instance_id := <<InstanceId/binary>>}} = vt_endpoint:received_from_probe(summary_info),

  % wait until tracing is fully enabled
  {response, _, apply_config, ok} = vt_endpoint:received_from_probe(apply_config),

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



receive_http_request_event_and_delta(Config) ->
  InstanceId = proplists:get_value(app_instance_id, Config),
  UserId = proplists:get_value(app_user_id, Config),

  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),
  BaseUrl = proplists:get_value(app_base_url, Config),

  {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, BaseUrl, [], <<>>, []),
  ok = hackney:skip_body(ClientRef),

  {events, _} = vt_endpoint:received_from_probe(events),

  % open websocket and subscribe to instance delta
  % as browser client would do
  {ok, Ws} = vt_endpoint:ws_connect(),
  ok = vt_endpoint:ws_send(Ws, subscribe_to_instance, InstanceId),

  #{<<"plug:requests">> := Reqs} = vt_endpoint:ws_delivered(Ws, delta),

  % key of the request is unknown
  [{ReqId, Req1}] = maps:to_list(Reqs),
  #{
    <<"StartedAt">> := StartedAt, <<"StoppedAt">> := StoppedAt, <<"Pid">> := _,
    <<"resp_code">> := <<"200">>, <<"method">> := <<"GET">>, <<"path">> := <<"/">>
  } = Req1,
  true = StartedAt < StoppedAt,



  {ok, ReqInfo} = vt:api_request(get, plug_request, #{instance_id => InstanceId, request_id => ReqId}),
  #{
    <<"StartedAt">> := StartedAt, <<"StoppedAt">> := StoppedAt, <<"Pid">> := _,
    <<"resp_code">> := <<"200">>, <<"method">> := <<"GET">>, <<"path">> := <<"/">>,
    <<"plugs">> := Plugs, <<"resp_headers">> := RespHeaders, <<"req_headers">> := ReqHeaders
  } = ReqInfo,

  [
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.Static">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.RequestId">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.Logger">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.Parsers">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.MethodOverride">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.Head">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.Session">>},
    #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.PhoenixApp1Web.Router">>, <<"plugs">> := [
      #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.Plug.CSRFProtection">>},
      #{<<"StartedAt">> := _, <<"StoppedAt">> := _, <<"module">> := <<"Elixir.PhoenixApp1Web.PageController">>}
    ]}
  ] = Plugs,

  % just to ensure lexicographic order
  ReqHeaders1 = lists:sort(ReqHeaders),
  RespHeaders1 = lists:sort(RespHeaders),

  [[<<"host">>, <<_/binary>>], [<<"user-agent">>, <<"hackney", _/binary>>] | _] = ReqHeaders1,
  [[<<"cache-control">>, <<_/binary>>], [<<"content-type">>, <<"text/html", _/binary>>] | _] = RespHeaders1,

  ok.


