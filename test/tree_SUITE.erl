-module(tree_SUITE).
-export([
  all/0, groups/0,
  init_per_suite/1, end_per_suite/1
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
  [{group, main}].

groups() ->
  [
    {main, [parallel], [
      spawn_process,
      exit_with_reason
    ]}
  ].



init_per_suite(Config) ->
  application:ensure_all_started(batiscaph),

  Port = 12345,
  {ok, ContainerPid} = bt_container:start_link(<<"batiscaph-test/erlang17_app1:latest">>, #{
    logdir => list_to_binary(proplists:get_value(priv_dir, Config)),
    name => ?MODULE, host_network => true, <<"HTTP_PORT">> => Port,
    <<"BATISCAPH_PROBE_ENDPOINT_URL">> => bt:test_endpoint_url()
  }),

  % going to be stopped manually in end_per_suite
  unlink(ContainerPid),
  {ok, InstanceId} = wait_for_instance_id(5000),
  [{instance_id, InstanceId}, {client_port, Port}, {client_container_pid, ContainerPid} | Config].



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
% Tree match testcases
%



spawn_process(Config) ->
  timer:sleep(10000),
  {ok, #{pid := ReqPid, started_at := _, stopped_at := _}}
    = run_test(?FUNCTION_NAME, Config),

  NewState = #{pid => undefined},
  {ok, _} = match_tree(fun

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

  end, NewState, Config),

  ok.



exit_with_reason(Config) ->
  {ok, #{pid := ReqPid, started_at := _, stopped_at := _}}
    = run_test(?FUNCTION_NAME, Config),

  NewState = #{pid => undefined},
  {ok, _} = match_tree(fun

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

  end, NewState, Config),
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



% it tries to match every fresh delta against MatchFunc
% until timeout
match_tree(MatchFunc, MatchState, CtConfig) ->
  match_tree(MatchFunc, MatchState, CtConfig, #{timeout => 10000}).



match_tree(MatchFunc, MatchState, CtConfig, #{timeout := Timeout}) ->
  InstanceId = proplists:get_value(instance_id, CtConfig),
  % connect to websocket using InstanceId
  {ok, WsPid} = bt:ws_connect(),
  ok = bt:ws_send(WsPid, subscribe_to_instance, #{id => InstanceId}),
  State = #{
    match_started_at => erlang:system_time(milli_seconds),
    websocket_pid => WsPid, timeout => Timeout
  },
  match_tree_loop(MatchFunc, MatchState, State).



match_tree_loop(MatchFunc, MatchState, State) ->
  #{match_started_at := StartedAt, timeout := Timeout, websocket_pid := WsPid} = State,
  case erlang:system_time(milli_seconds) - StartedAt of
    TimeSpent when TimeSpent > Timeout -> {error, timeout};
    TimeSpent ->
      TimeLeft = Timeout - TimeSpent,

      case bt:ws_receive(WsPid, delta, TimeLeft) of
        {error, timeout} ->
          ct:pal("match state: ~p", [MatchState]),
          ct:pal("tree: ~p", [maps:get(last_tree, State, undefined)]),
          error(delta_didnt_match);

        {ok, Delta} ->
          % currently we receive whole tree each time instead of deltas
          % in future here should be done a merge with all previously received deltas
          Tree = atomize_delta(Delta),
          try MatchFunc(Tree, MatchState) of
            {more, MatchState1} -> match_tree_loop(MatchFunc, MatchState1, State#{last_tree => Tree});
            {done, MatchState1} ->
              ct:pal("successful match, state: ~p", [MatchState1]),
              {ok, MatchState1}
          catch
            error:function_clause -> match_tree_loop(MatchFunc, MatchState, State#{last_tree => Tree})
          end
      end
  end.



atomize_delta(Delta) ->
  % turn first level keys into atoms
  maps:fold(fun (K, V, Acc) ->
    Acc#{binary_to_atom(K,latin1) => V}
  end, #{}, Delta).
