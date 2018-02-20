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
  Opts = #{bind_ports => [Port], <<"VISION_ENDPOINT_PORT">> => Port},
  {ok, EndpointContainer} = vt:start_docker_container(<<"endpoint1">>, <<"vision/endpoint:latest">>, Opts),
  ProbeOpts = #{endpoint_url => endpoint_url(vt_container:node(EndpointContainer), Port)},
  [{endpoint_container, EndpointContainer}, {probe_opts, ProbeOpts} | Config].

endpoint_url(EndpointNode, Port) ->
  [_Name, Host] = binary:split(atom_to_binary(EndpointNode,latin1), <<"@">>),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port), "/probe"]).

stop_endpoint(Config) ->
  EndpointContainer = proplists:get_value(endpoint_container, Config),
  ok = vt:stop_docker_container(EndpointContainer),
  ok.



init_per_group(example_app1, Config) ->
  % start docker container with example_app1
  Config;
init_per_group(_Group, Config) -> Config.



end_per_group(example_app1, Config) ->
  % terminate container
  Config;
end_per_group(_, Config) -> Config.



test1(Config) ->
  timer:sleep(10000),
  ok.
