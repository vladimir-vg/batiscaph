-module(shell_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  creates_context_with_var_mentions/1
]).



all() ->
  [
    creates_context_with_var_mentions
  ].



init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),
  Config.

  % InstanceId = bt:g(instance_id, query_SUITE),
  % {ok, Pid} = feed_events(InstanceId),
  % unlink(Pid), % gonna be terminated in end_per_suite
  % 
  % [{instance_id, InstanceId}, {remote_ctl_pid, Pid} | Config].



end_per_suite(Config) ->
  Config.



creates_context_with_var_mentions(_Config) ->
  % should start shell on local node
  InstanceId = bt:g(instance_id, shell_SUITE),
  {ok, Pid} = remote_ctl:start_link(InstanceId, #{}),

  ok = gen_server:call(Pid, {subscribe_websocket, self(), #{}}),

  receive {shell_input, {ready, _Prompt}} -> ok
  after 1000 -> error(expected_shell_to_be_ready)
  end,

  {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
  ScenarioPid ! {shell_input, <<"Self = self(), FileServer = whereis(file_server_2).">>},

  % make sure that message is processed
  ok = gen_server:call(ScenarioPid, sync),
  ok = gen_server:call(Pid, sync),

  {ok, Delta} = remote_ctl:delta_json(#{instance_id => InstanceId}),
  % expect to have context for shell
  #{contexts := Contexts, events := Events} = bt:atomize_delta(Delta),

  % we don't know what key is gonna be called,
  % but shell should to produce only one context
  [{_Key, Context}] = maps:to_list(Contexts),
  #{variables := #{<<"Self">> := SelfPid, <<"FileServer">> := FileServerPid}} = Context,
  Events1 = [E || #{type := <<"VAR_MENTION">>} = E <- Events],
  [#{expr := <<"Self">>, pid2 := SelfPid}, #{expr := <<"FileServer">>, pid2 := FileServerPid}] = Events1,

  ok.
