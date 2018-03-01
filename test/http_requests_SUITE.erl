-module(http_requests_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([receive_http_request_event/1]).



all() ->
  [receive_http_request_event].



init_per_suite(Config) ->
  % gonna need this for making requests to test app
  application:ensure_all_started(hackney),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_fresh_endpoint_running(#{logdir => PrivDir}),
  ok = vt:ensure_fresh_webapp_running(#{logdir => PrivDir}),

  Port = 8083,
  {ok, UserId, AppContainer} = start_webapp(PrivDir, Port),
  BaseUrl = vt:base_url(AppContainer, Port),

  [{app_base_url, BaseUrl}, {app_user_id, UserId} | Config].

end_per_suite(Config) ->
  Config.



start_webapp(PrivDir, Port) ->
  WebappNode = vt:webapp_node(),
  {ok, UserId, AccessKey} = rpc:call(WebappNode, 'Elixir.Vision.Test', create_user_and_return_access_key, []),

  {ok, AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/phoenix_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"DATABASE_URL">> => <<"postgres://postgres:postgres@127.0.0.1/vision_test_phoenix_app1">>,
    <<"HTTP_PORT">> => Port,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey,
    runtype => phoenix,
    wait_for_application => phoenix_app1
  }),
  {ok, UserId, AppContainer}.



receive_http_request_event(Config) ->
  UserId = proplists:get_value(user_id, Config),
  EndpointNode = vt:endpoint_node(),

  ok = rpc:call(EndpointNode, vision_test, subscribe_to_first_session, [self(), #{user_id => UserId}]),

  BaseUrl = proplists:get_value(app_base_url, Config),

  {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, BaseUrl, [], <<>>, []),
  ok = hackney:skip_body(ClientRef),

  {events, _Events} = vt:received_from_probe(events),

  ok.
