-module(tree_SUITE).
-export([
  all/0, groups/0,
  init_per_suite/1, end_per_suite/1,
  init_per_group/2, end_per_group/2
]).
-export([
  spawn_process/1, exit_with_reason/1
]).



%
% Tests in this module work in following way:
% 
%   1. Start client node in container with test code,
%      probe and http server.
%
%   2. Testcase makes request to http server,
%      asks to execute function from tree_testcases module.
%      Receives remote pid of process executing test,
%      start and stop timings.
%
%  3. Testcase tries to match against tree (merged from received delta)
%     until matching function returns {done, _} or timeout
%



all() ->
  [{group, erlang17_app1}, {group, erlang18_app1}].

groups() ->
  Testcases = [
    spawn_process,
    exit_with_reason
  ],
  [
    {erlang17_app1, [parallel], Testcases},
    {erlang18_app1, [parallel], Testcases}
  ].



init_per_suite(Config) ->
  application:ensure_all_started(batiscaph),
  Config.

end_per_suite(Config) ->
  Config.



init_per_group(erlang17_app1, Config) ->
  Port = 12017,
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang17_app1:latest">>, Port, Config),
  [{instance_id, InstanceId}, {client_port, Port}, {client_container_pid, ContainerPid} | Config];

init_per_group(erlang18_app1, Config) ->
  Port = 12018,
  {ok, InstanceId, ContainerPid} = start_erlang_node(<<"batiscaph-test/erlang18_app1:latest">>, Port, Config),
  [{instance_id, InstanceId}, {client_port, Port}, {client_container_pid, ContainerPid} | Config].



end_per_group(_, Config) ->
  ContainerPid = proplists:get_value(client_container_pid, Config),
  exit(ContainerPid, end_per_suite),
  Config.



start_erlang_node(ImageName, Port, Config) ->
  {ok, ContainerPid} = bt_container:start_link(ImageName, #{
    logdir => list_to_binary(proplists:get_value(priv_dir, Config)),
    name => ?MODULE, host_network => true, <<"HTTP_PORT">> => Port,
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
% Tree match testcases
%



spawn_process(Config) ->
  {ok, #{pid := ReqPid, started_at := _, stopped_at := _}}
    = run_test(?FUNCTION_NAME, Config),

  MatchOpts = #{instance_id => proplists:get_value(instance_id, Config)},

  NewState = #{pid => undefined},
  {ok, _} = bt:match_tree(fun

    (#{erlang_processes := #{ReqPid := #{<<"Children">> := Children}}},
     #{pid := undefined} = State)
    when 1 == map_size(Children)
    ->
      [Pid] = maps:keys(Children),
      {more, State#{pid => Pid}};

    (#{erlang_processes := Procs},
     #{pid := Pid} = State)
    ->
      case maps:get(Pid, Procs, undefined) of
        #{<<"SpawnedAt">> := At1, <<"ExitedAt">> := At2} when At1 < At2 ->
          {done, State};
        _ ->
          {more, State}
      end

  end, NewState, MatchOpts),

  ok.



exit_with_reason(Config) ->
  {ok, #{pid := ReqPid, started_at := _, stopped_at := _}}
    = run_test(?FUNCTION_NAME, Config),

  MatchOpts = #{instance_id => proplists:get_value(instance_id, Config)},

  NewState = #{pid => undefined},
  {ok, _} = bt:match_tree(fun

    (#{erlang_processes := #{ReqPid := #{<<"Children">> := Children}}},
     #{pid := undefined} = State)
    when 1 == map_size(Children)
    ->
      [Pid] = maps:keys(Children),
      {more, State#{pid => Pid}};

    (#{erlang_processes_info := Infos},
     #{pid := Pid} = State)
    ->
      case maps:get(Pid, Infos, undefined) of
        #{<<"ExitReason">> := <<"reason1">>} -> {done, State};
        _ -> {more, State}
      end

  end, NewState, MatchOpts),
  ok.



%
% Helpers
%



run_test(TC, CtConfig) when is_atom(TC) ->
  Port = proplists:get_value(client_port, CtConfig),
  BaseUrl = <<"http://0.0.0.0:", (integer_to_binary(Port))/binary, "/tree_testcases/">>,
  Url = <<BaseUrl/binary, (atom_to_binary(TC, latin1))/binary>>,

  {ok, 200, RespHeaders, Body} = hackney:request(post, Url, [], <<>>, [with_body]),

  Pid = proplists:get_value(<<"x-pid">>, RespHeaders),
  is_binary(Pid) orelse error({bad_resp_headers, RespHeaders}),
  StartedAt = proplists:get_value(<<"x-started-at">>, RespHeaders),
  StoppedAt = proplists:get_value(<<"x-stopped-at">>, RespHeaders),

  Result = #{
    pid => Pid, started_at => binary_to_integer(StartedAt),
    stopped_at => binary_to_integer(StoppedAt)
  },
  ct:pal("testcase request: ~p~n~s", [Result, Body]),
  {ok, Result}.
