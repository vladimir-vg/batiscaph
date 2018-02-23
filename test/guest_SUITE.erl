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
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  Opts = #{
    <<"VISION_ENDPOINT_HTTP_PORT">> => Port,
    host_network => true,
    logdir => PrivDir
  },
  {ok, EndpointContainer} = vt:start_docker_container(<<"endpoint1">>, <<"vision/endpoint:latest">>, Opts),
  EndpointUrl = endpoint_url(vt_container:node(EndpointContainer), Port),

  ok = wait_for_application(vt_container:node(EndpointContainer), batiscaph, 5000),

  [{endpoint_container, EndpointContainer}, {endpoint_url, EndpointUrl} | Config].

endpoint_url(EndpointNode, Port) ->
  Host = hostname_from_node(EndpointNode),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port), "/probe"]).

wait_for_application(_, _, Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_application(Node, App, Timeout) ->
  Apps = rpc:call(Node, application, which_applications, []),
  case lists:keyfind(App, 1, Apps) of
    false -> timer:sleep(100), wait_for_application(Node, App, Timeout-100);
    {App, _Version, _Desc} -> ok
  end.

stop_endpoint(Config) ->
  EndpointContainer = proplists:get_value(endpoint_container, Config),
  ok = vt:stop_docker_container(EndpointContainer),
  ok.

hostname_from_node(Node) ->
  [_Name, Host] = binary:split(atom_to_binary(Node,latin1), <<"@">>),
  Host.



init_per_group(guest_app_run, Config) ->
  EndpointUrl = proplists:get_value(endpoint_url, Config),
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  % do not start app immediately
  Opts = #{autostart => false, host_network => true, <<"VISION_PROBE_ENDPOINT_URL">> => EndpointUrl, logdir => PrivDir},
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
  EndpointNode = vt_container:node(proplists:get_value(endpoint_container, Config)),

  % subscribe for communication
  ok = rpc:call(EndpointNode, vision_test, subscribe_to_first_guest, [self()]),

  % now when we subscribed for communication
  % we can start node, listen and match
  ok = vt_container:start_node(AppContainer),

  receive
    {from_probe, Message} ->
      #{
        probe_version := <<"0.1.0">>,
        dependency_in := [{<<"example_app1">>, <<"1.2.3-test1">>}],
        instance_id := <<_/binary>>
      } = Message,
      ok
  after 5000 ->
    ct:pal("messages: ~p", [process_info(self(), messages)]),
    error(bad_message_match)
  end,

  ok.
