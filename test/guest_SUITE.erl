-module(guest_SUITE).
-export([
  all/0, groups/0, init_per_suite/1, end_per_suite/1,
  init_per_group/2, end_per_group/2
]).
-export([receive_version/1]).



all() ->
  [{group, guest_app_run}].

groups() ->
  [{guest_app_run, [], [
    receive_version
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
  Host = hostname_from_node(EndpointNode),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port), "/probe"]).

stop_endpoint(Config) ->
  EndpointContainer = proplists:get_value(endpoint_container, Config),
  ok = vt:stop_docker_container(EndpointContainer),
  ok.

hostname_from_node(Node) ->
  [_Name, Host] = binary:split(atom_to_binary(Node,latin1), <<"@">>),
  Host.



init_per_group(guest_app_run, Config) ->
  EndpointUrl = proplists:get_value(endpoint_url, Config),
  % do not start app immediately
  Opts = #{autostart => false, <<"VISION_ENDPOINT_URL">> => EndpointUrl},
  {ok, AppContainer} = vt:start_docker_container(<<"guest_app_run">>, <<"vision-test/example_app1:latest">>, Opts),
  [{app_container, AppContainer} | Config];

init_per_group(_Group, Config) ->
  Config.



end_per_group(guest_app_run, Config) ->
  AppContainer = proplists:get_value(app_container, Config),
  ok = vt:stop_docker_container(AppContainer),
  Config;

end_per_group(_, Config) ->
  Config.



receive_version(Config) ->
  AppContainer = proplists:get_value(app_container, Config),
  AppNode = vt_container:node(AppContainer),
  EndpointNode = vt_container:node(proplists:get_value(endpoint_container, Config)),

  % subscribe for communication
  AppHostname = hostname_from_node(AppNode),
  ok = rpc:call(EndpointNode, vi_receiver, subscribe_to_first_guest_from_ip, [self(), AppHostname]),

  % now when we subscribed for communication
  % we can start node, listen and match
  ok = vt_container:start_node(AppContainer),

  receive
    {from_probe, Message} ->
      {probe_guest_info, Info} = Message,
      #{
        probe_version := <<"0.1.0">>,
        dependency := {<<"example_app1">>, <<"1.2.3-test1">>},
        instance_id := <<_/binary>>
      } = Info,
      ok
  after 5000 ->
    error(bad_message_match)
  end,

  ok.
