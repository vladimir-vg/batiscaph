-module(cases_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([open_and_close_port/1, register_port/1, link_port_to_other_process/1, change_port_owner/1]).



% This suite executes different functions,
% check resulting list of events and delta



all() ->
  [{group, main}].

groups() ->
  [{main, [parallel], [
    open_and_close_port,
    register_port,
    link_port_to_other_process,
    change_port_owner
  ]}].



init_per_suite(Config) ->
  % do expect that all necessary ENV variables were specified in advance
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),

  % starting client scenario on very same node
  Id = iolist_to_binary([atom_to_binary(?MODULE,latin1),"/",integer_to_binary(rand:uniform(1000000))]),
  {ok, Pid} = remote_ctl:start_link(Id, #{node => node()}),
  ok = wait_for_collector_to_appear(300),

  [{remote_ctl_pid, Pid}, {instance_id, Id} | Config].

end_per_suite(Config) ->
  Config.



wait_for_collector_to_appear(Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_collector_to_appear(Timeout) ->
  case whereis(z__client_collector) of
    undefined -> timer:sleep(5), wait_for_collector_to_appear(Timeout-5);
    Pid when is_pid(Pid) -> ok
  end.



open_and_close_port(Config) ->
  {ok, Events, Delta} = trace_case(Config, fun () ->
    Port = erlang:open_port({spawn_executable, "/bin/cat"}, []),
    true = erlang:port_close(Port)
  end),

  PidBin = list_to_binary(pid_to_list(self())),
  {ok, Events1} = take_subseq(#{pid => PidBin}, [trace_started, port_open, port_close, trace_stopped], Events),
  [_, #{type := <<"port_open">>, port := PortBin}, #{type := <<"port_close">>, port := PortBin}, _] = Events1,

  #{ports := #{PortBin := #{parts := [#{pid := PidBin, startedAt := _, stoppedAt := _}], events := []}}} = Delta,
  ok.



register_port(_Config) ->
  {skip, not_implemented}.



link_port_to_other_process(_Config) ->
  {skip, not_implemented}.



change_port_owner(_Config) ->
  {skip, not_implemented}.



%%% helper functions



trace_case(Config, Fun) when is_function(Fun) ->
  Pid = proplists:get_value(remote_ctl_pid, Config),
  InstanceId = proplists:get_value(instance_id, Config),

  T1 = erlang:system_time(micro_seconds),
  ok = z__client_scenario:trace_pid(self()),
  Fun(),
  ok = z__client_scenario:clear_tracing(self()),
  T2 = erlang:system_time(micro_seconds),

  {ok, Events} = clk_events:select(#{instance_id => InstanceId, 'after' => T1, before => T2}),
  {ok, Delta} = remote_ctl:delta_json(#{instance_id => InstanceId, 'after' => T1, before => T2}),
  {ok, Events, Delta}.

