-module(cases_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([open_and_close_port/1, register_port/1, link_port_to_other_process/1, change_port_owner/1, change_port_owner_by_message/1]).



% This suite executes different functions,
% check resulting list of events and delta



all() ->
  [{group, main}].

groups() ->
  [{main, [parallel], [
    open_and_close_port,
    register_port,
    link_port_to_other_process,
    change_port_owner,
    change_port_owner_by_message
  ]}].



init_per_suite(Config) ->
  % do expect that all necessary ENV variables were specified in advance
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),

  % starting client scenario on very same node
  Id = iolist_to_binary([atom_to_binary(?MODULE,latin1),"/",integer_to_binary(rand:uniform(1000000))]),
  {ok, Pid} = remote_ctl:start_link(Id, #{node => node()}),
  unlink(Pid),
  ok = bt:wait_for_collector_to_appear(),

  [{remote_ctl_pid, Pid}, {instance_id, Id} | Config].

end_per_suite(Config) ->
  Pid = proplists:get_value(remote_ctl_pid, Config),

  % collector consumed all trace messages
  ok = gen_server:call(z__client_collector, flush),
  % remote_ctl consumed all event messages
  ok = gen_server:call(Pid, sync),

  exit(Pid, kill),
  exit(whereis(z__client_sup), kill),
  Config.



open_and_close_port(Config) ->
  {ok, Events, Delta} = trace_case(Config, fun () ->
    Port = erlang:open_port({spawn_executable, "/bin/cat"}, []),
    true = erlang:port_close(Port)
  end),

  % PidBin = list_to_binary(pid_to_list(self())),
  % {ok, Events1} = take_subseq(#{pid => PidBin}, [trace_started, port_open, port_close, trace_stopped], Events),
  % [_, #{type := <<"port_open">>, port := PortBin}, #{type := <<"port_close">>, port := PortBin}, _] = Events1,
  % 
  % #{ports := #{PortBin := #{parts := [#{pid := PidBin, startedAt := _, stoppedAt := _}], events := []}}} = Delta,
  ok.



register_port(_Config) ->
  {skip, not_implemented}.



link_port_to_other_process(_Config) ->
  {skip, not_implemented}.



change_port_owner(Config) ->
  {ok, Events, Delta} = trace_case(Config, fun () ->
    Port = erlang:open_port({spawn_executable, "/bin/cat"}, []),

    Pid = erlang:spawn_link(fun () ->
      receive
        {Parent, close_port} ->
          erlang:port_close(Port),
          Parent ! {self(), done}
      end
    end),

    % now Pid is owner of the Port
    true = erlang:port_connect(Port, Pid),
    Pid ! {self(), close_port},
    receive
      {Pid, done} -> ok
    after 1000 ->
      error(timeout_port_close)
    end,

    % check that it closed
    undefined = erlang:port_info(Port)
  end),

  % SelfBin = list_to_binary(pid_to_list(self())),
  % {ok, Events1} = take_subseq(#{pid => SelfBin}, [trace_started, port_open, port_change_owner, trace_stopped], Events),
  % [_, #{type := <<"port_open">>, port := PortBin}, #{type := <<"port_close">>, port := PortBin}, _] = Events1,
  % 
  % #{ports := #{PortBin := #{parts := [#{pid := PidBin, startedAt := _, stoppedAt := _}], events := []}}} = Delta,
  ok.



change_port_owner_by_message(Config) ->
  {ok, Events, Delta} = trace_case(Config, fun () ->
    Port = erlang:open_port({spawn_executable, "/bin/cat"}, []),

    Pid = erlang:spawn_link(fun () ->
      receive
        {Parent, close_port} ->
          erlang:port_close(Port),
          Parent ! {self(), done}
      end
    end),

    % now Pid is owner of the Port
    Port ! {self(), {connect, Pid}},
    receive {Port, connected} -> ok
    after 1000 -> error(expected_to_change_owner)
    end,

    Pid ! {self(), close_port},
    receive
      {Pid, done} -> ok
    after 1000 ->
      error(timeout_port_close)
    end,

    % check that it closed
    undefined = erlang:port_info(Port)
  end),
  ok.



%%% helper functions



trace_case(Config, Fun) when is_function(Fun) ->
  Pid = proplists:get_value(remote_ctl_pid, Config),
  InstanceId = proplists:get_value(instance_id, Config),

  T1 = erlang:system_time(micro_seconds),
  ok = z__client_scenario:trace_pid(self()),

  Fun(),

  Ref = erlang:trace_delivered(self()),
  ok = z__client_scenario:clear_tracing(self()),
  T2 = erlang:system_time(micro_seconds),

  % make sure that up to this point in time:
  % all trace messages are delivered
  receive {trace_delivered, _, Ref} -> ok
  after 1000 -> error(timeout_trace_delivered)
  end,
  % collector consumed all trace messages
  ok = gen_server:call(z__client_collector, flush),
  % remote_ctl consumed all event messages
  ok = gen_server:call(Pid, sync, 20000),

  {ok, Events} = clk_events:select(#{instance_id => InstanceId, 'after' => T1, before => T2}),
  {ok, Delta} = remote_ctl:delta_json(#{instance_id => InstanceId, 'after' => T1, before => T2}),
  {ok, bt:atomize_events(Events), bt:atomize_delta(Delta)}.



take_subseq(Filter, EventTypes, Events) ->
  Filter1 = maps:to_list(Filter),
  EventTypes1 = [atom_to_binary(T,latin1) || T <- EventTypes],
  Events1 = lists:filter(fun (#{} = E) ->
    % check that all key-value pairs presented in E
    lists:all(fun ({K, V}) ->
      maps:get(K, E, undefined) == V
    end, Filter1)
  end, Events),
  take_subseq0(EventTypes1, Events1, []).

take_subseq0([], _Events, Acc) -> {ok, lists:reverse(Acc)};
take_subseq0(Types, [], Acc) -> {error, {unmatched_types_left, Types, lists:reverse(Acc)}};
take_subseq0([T | Types], [#{type := T} = E | Events], Acc) ->
  take_subseq0(Types, Events, [E | Acc]);
take_subseq0(Types, [_ | Events], Acc) ->
  take_subseq0(Types, Events, Acc).
