-module(ct_SUITE).
-export([
  all/0, groups/0,
  init_per_suite/1, end_per_suite/1,
  init_per_group/2, end_per_group/2
]).
-export([
  empty_suite/1
]).



%
% Tests in this suite run CT suite of the test app in container
% with batiscaph ct hook and match against produced tree.
%



all() ->
  [{group, erlang17_app1}, {group, erlang18_app1}, {group, erlang19_app1}, {group, erlang20_app1}].

groups() ->
  Testcases = [
    empty_suite
  ],
  [
    {erlang17_app1, [parallel], Testcases},
    {erlang18_app1, [parallel], Testcases},
    {erlang19_app1, [parallel], Testcases},
    {erlang20_app1, [parallel], Testcases}
  ].



init_per_suite(Config) ->
  application:ensure_all_started(batiscaph),
  Config.

end_per_suite(Config) ->
  Config.



init_per_group(erlang17_app1, Config) ->
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang17_app1:latest">>, Config),
  [{instance_id, InstanceId}, {client_container_pid, ContainerPid} | Config];
init_per_group(erlang18_app1, Config) ->
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang18_app1:latest">>, Config),
  [{instance_id, InstanceId}, {client_container_pid, ContainerPid} | Config];
init_per_group(erlang19_app1, Config) ->
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang19_app1:latest">>, Config),
  [{instance_id, InstanceId}, {client_container_pid, ContainerPid} | Config];
init_per_group(erlang20_app1, Config) ->
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang20_app1:latest">>, Config),
  [{instance_id, InstanceId}, {client_container_pid, ContainerPid} | Config].



end_per_group(_, Config) ->
  ContainerPid = proplists:get_value(client_container_pid, Config),
  exit(ContainerPid, end_per_suite),
  Config.



start_erlang_node(ImageName, Config) ->
  GroupName = proplists:get_value(name, proplists:get_value(tc_group_properties, Config)),
  {ok, ContainerPid} = bt_container:start_link(ImageName, #{
    logdir => list_to_binary(proplists:get_value(priv_dir, Config)),
    name => iolist_to_binary([atom_to_binary(GroupName,latin1), "_", atom_to_binary(?MODULE,latin1)]),
    host_network => true, cmd => [<<"./rebar3">>, <<"ct">>],
    <<"BATISCAPH_PROBE_ENDPOINT_URL">> => bt:test_endpoint_url()
  }),
  % going to be stopped manually in end_per_suite
  unlink(ContainerPid),
  {ok, InstanceId} = wait_for_instance_id(5000),
  {ok, InstanceId, ContainerPid}.



wait_for_instance_id(Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_instance_id(Timeout) ->
  case gen_tracker:list(probes) of
    [] -> timer:sleep(100), wait_for_instance_id(Timeout-100);
    [{InstanceId, _Attrs}] -> {ok, InstanceId};
    List -> {error, {unexpected_existing_instances, List}}
  end.



%
% Common test testcases
%



empty_suite(Config) ->
  MatchOpts = #{instance_id => proplists:get_value(instance_id, Config)},

  {ok, _} = bt:match_tree(fun

    (#{ct_suites := #{<<"empty_SUITE">> := #{<<"Testcases">> := #{
      <<"testcase1">> := #{<<"StartedAt">> := _, <<"StoppedAt">> := _}
    }}}},
     State)
    ->
      {done, State}

  end, no_state, MatchOpts),
  ok.
