-module(basic_SUITE).
-export([
  all/0, groups/0, init_per_suite/1, end_per_suite/1,
  init_per_group/2, end_per_group/2
]).
-export([test1/1]).



all() ->
  [{group, example_app1}].

groups() ->
  [{example_app1, [parallel], [
    test1
  ]}].



init_per_suite(Config) ->
  Config1 = start_endpoint(8080, Config),
  Config1.

end_per_suite(Config) ->
  ok = stop_endpoint(Config),
  Config.



start_endpoint(Port, Config) ->
  Opts = #{<<"VISION_ENDPOINT_PORT">> => Port},
  {ok, EndpointContainer} = vt:start_docker_container(<<"endpoint1">>, <<"vision/endpoint:latest">>, Opts),
  EndpointUrl = endpoint_url(vt_container:node(EndpointContainer), Port),
  [{endpoint_container, EndpointContainer}, {endpoint_url, EndpointUrl} | Config].

endpoint_url(EndpointNode, Port) ->
  [_Name, Host] = binary:split(atom_to_binary(EndpointNode,latin1), <<"@">>),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port), "/probe"]).

stop_endpoint(Config) ->
  EndpointContainer = proplists:get_value(endpoint_container, Config),
  ok = vt:stop_docker_container(EndpointContainer),
  ok.



init_per_group(example_app1, Config) ->
  EndpointUrl = proplists:get_value(endpoint_url, Config),
  Opts = #{<<"VISION_ENDPOINT_URL">> => EndpointUrl},
  {ok, AppContainer} = vt:start_docker_container(<<"example_app1">>, <<"vision-test/example_app1:latest">>, Opts),
  [{app_container, AppContainer} | Config];

init_per_group(_Group, Config) ->
  Config.



end_per_group(example_app1, Config) ->
  AppContainer = proplists:get_value(app_container, Config),
  ok = vt:stop_docker_container(AppContainer),
  Config;

end_per_group(_, Config) ->
  Config.



test1(Config) ->
  AppNode = vt_container:node(proplists:get_value(app_container, Config)),
  EndpointNode = vt_container:node(proplists:get_value(endpoint_container, Config)),
  AppResp = rpc:call(AppNode, example_app1_app, module_info, []),
  EndResp = rpc:call(EndpointNode, batiscaph_app, module_info, []),
  ct:pal("responses ~p~n~p~n", [AppResp, EndResp]),
  ok.
