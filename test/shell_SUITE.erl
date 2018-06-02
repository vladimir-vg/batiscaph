-module(shell_SUITE).
-export([
  all/0, groups/0,
  init_per_suite/1, end_per_suite/1,
  init_per_group/2, end_per_group/2
]).
-export([
	subscribe_to_process_info/1
]).



%
% Tests in this suite send shell commands and match tree changes.
%



all() ->
  [{group, erlang17_app1}, {group, erlang18_app1}, {group, erlang19_app1}, {group, erlang20_app1}].

groups() ->
  Testcases = [
    subscribe_to_process_info
  ],
  [
    {erlang17_app1, [shuffle], Testcases},
    {erlang18_app1, [shuffle], Testcases},
    {erlang19_app1, [shuffle], Testcases},
    {erlang20_app1, [shuffle], Testcases}
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
    host_network => true,
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
% Shell execution testcases
%



subscribe_to_process_info(Config) ->
  InstanceId = proplists:get_value(instance_id, Config),
  MatchOpts = #{websocket_pid := WsPid} = fresh_ws_shell(InstanceId),

  {ok, _} = shell_exec("f().", MatchOpts), % forget variables created by other testcasees
  {ok, Output1} = shell_exec("Pid = spawn(fun () -> receive stop -> ok end end).", MatchOpts),
  Pid = string:trim(Output1),
  ct:pal("Pid: ~p", [Pid]),

  {ok, _} = bt:match_tree(fun
    (#{erlang_processes := #{Pid := #{<<"SpawnedAt">> := _}}}, State) ->
      {done, State}
  end, no_state, MatchOpts),

  ok = bt:ws_send(WsPid, subscribe_to_process_info, #{instance_id => InstanceId, pid => Pid}),

  {ok, _} = bt:match_tree(fun
    (#{erlang_processes_info := #{Pid := #{<<"Changes">> := #{}}}}, State) ->
      {done, State}
  end, no_state, MatchOpts),

  {ok, _} = shell_exec("Pid ! stop.", MatchOpts),

  {ok, _} = bt:match_tree(fun
    (#{erlang_processes := #{Pid := #{<<"ExitedAt">> := _}}}, State) ->
      {done, State}
  end, no_state, MatchOpts),

  ok.



%
% Helpers
%



fresh_ws_shell(InstanceId) ->
  {ok, WsPid} = bt:ws_connect(),
  ok = bt:ws_send(WsPid, subscribe_to_instance, #{id => InstanceId}),
  ok = bt:ws_send(WsPid, connect_to_shell, #{id => InstanceId}),
  ok = bt:ws_receive(WsPid, connected_to_shell),
  #{websocket_pid => WsPid, instance_id => InstanceId}.



shell_exec(Input, #{websocket_pid := WsPid, instance_id := InstanceId} = MatchOpts) ->
  Input1 = iolist_to_binary(Input),
  InputLength = byte_size(Input1),
  ok = bt:ws_send(WsPid, shell_input, #{id => InstanceId, text => Input1}),

  % just mathing for the shell command that has exactly same input text
  % then just grab all outputs and concat them
  bt:match_tree(fun
    (#{shell_commands := Cmds}, _State) ->
      maps:fold(fun
        (_, #{<<"Input">> := <<Input2:InputLength/binary, "\n">>, <<"Outputs">> := Outputs}, _Acc)
        when Input1 =:= Input2 ->
          Values = lists:sort(fun
            (#{<<"At">> := A}, #{<<"At">> := B}) -> A =< B
          end, maps:values(Outputs)),
          Output = iolist_to_binary([Text || #{<<"Text">> := Text} <- Values]),
          {done, Output};

        (_, _, _Acc) ->
          {more, no_state}
      end, {ok, no_state}, Cmds)
  end, no_state, MatchOpts).
