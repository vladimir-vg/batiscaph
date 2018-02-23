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
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_endpoint_running(#{logdir => PrivDir}),
  Config.

end_per_suite(Config) ->
  Config.



init_per_group(guest_app_run, Config) ->
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  % do not start app immediately
  Opts = #{
    autostart => false, host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url()
  },
  {ok, AppContainer} = vt:start_docker_container(<<"guest_app_run">>, <<"vision-test/example_app1:latest">>, Opts),
  [{app_container, AppContainer} | Config];

init_per_group(_Group, Config) ->
  Config.



end_per_group(_, Config) ->
  Config.



receive_version(Config) ->
  AppContainer = proplists:get_value(app_container, Config),
  EndpointNode = vt:endpoint_node(),

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
