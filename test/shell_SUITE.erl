-module(shell_SUITE).
-export([
  all/0, groups/0,
  init_per_suite/1, end_per_suite/1
]).
-export([
	subscribe_to_process_info/1
]).



%
% Tests in this suite send shell commands and match tree changes.
%



all() ->
  [{group, main}].

groups() ->
  [
    {main, [shuffle], [
      subscribe_to_process_info
    ]}
  ].



init_per_suite(Config) ->
  application:ensure_all_started(batiscaph),

  {ok, ContainerPid} = bt_container:start_link(<<"batiscaph-test/erlang17_app1:latest">>, #{
    logdir => list_to_binary(proplists:get_value(priv_dir, Config)),
    name => ?MODULE, host_network => true,
    <<"BATISCAPH_PROBE_ENDPOINT_URL">> => bt:test_endpoint_url()
  }),

  % going to be stopped manually in end_per_suite
  unlink(ContainerPid),

  {ok, InstanceId} = wait_for_instance_id(5000),
  [{instance_id, InstanceId}, {client_container_pid, ContainerPid} | Config].



end_per_suite(Config) ->
  ContainerPid = proplists:get_value(client_container_pid, Config),
  exit(ContainerPid, end_per_suite),
  Config.



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
  {ok, WsPid} = bt:ws_connect(),
  ok = bt:ws_send(WsPid, subscribe_to_instance, #{id => InstanceId}),
  ok = bt:ws_send(WsPid, connect_to_shell, #{id => InstanceId}),
  ok = bt:ws_receive(WsPid, connected_to_shell),
  MatchOpts = #{websocket_pid => WsPid, instance_id => InstanceId},

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
