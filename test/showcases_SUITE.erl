-module(showcases_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([empty_test/1, file_open_test/1, ets_match_spec_transform/1, ets_mspec_with_records/1, open_port_and_change_owner/1, port_dies_with_bad_reason/1]).
-batiscaph_steps(all).
% -batiscaph_steps([empty_test, file_open_test, ets_match_spec_transform, ets_mspec_with_records, open_port_and_change_owner, port_dies_with_bad_reason]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("records.hrl").
-compile({parse_transform, batiscaph_suite_transform}).



%%% This test suite should be used only for display tests.
%%% Testing application logic should be done in other suites.
%%% Need to include all possible edge cases of displayed data.
%%% Would be great to test display of incorrect deltas (should somehow indicate that in UI).



init_per_suite(Config) ->
  % BatiscaphNode = list_to_atom("batiscaph@" ++ net_adm:localhost()),
  % application:set_env(batiscaph, batiscaph_node, BatiscaphNode),
  % case net_adm:ping(BatiscaphNode) of
  %   pong -> Config;
  %   pang -> {skip, {unable_to_connect_to_batiscaph_node, BatiscaphNode}}
  % end,
  Config.

end_per_suite(Config) ->
  % TODO: currently flush is executed during this last context
  % and as a result, it loses events about finishing this very context
  % Need flush somehow after executing this callback.
  % It can be done using ct hooks (after end_per_suite),

  % make sure that collector consumed all trace messages
  ok = gen_server:call(z__client_collector, flush),

  exit(whereis(z__client_sup), kill),
  Config.



init_per_group(group2, Config) ->
  special_clause_for_group2,
  Config;
init_per_group(_Group, Config) ->
  do_nothing,
  Config.



end_per_group(group2_nested, Config) ->
  other_special_clause,
  Config;
end_per_group(_, Config) ->
  do_nothing,
  Config.



init_per_testcase(TC, Config) when TC =:= empty_test orelse TC =:= something_else ->
  clause_for_test_case,
  Config;
init_per_testcase(_TC, Config) ->
  Config.

end_per_testcase(empty_test, Config) ->
  Config;
end_per_testcase(_, Config) ->
  Config.



all() ->
  [{group, main}, {group, group2}].

groups() ->
  [
    {main, [parallel], [
      empty_test,
      file_open_test,
      ets_match_spec_transform,
      ets_mspec_with_records,
      open_port_and_change_owner,
      port_dies_with_bad_reason
    ]},
    {group2, [parallel], [empty_test, {group, group2_nested}]},
    {group2_nested, [], [empty_test, {group, group3_nested}]},
    {group3_nested, [], [empty_test]}
  ].



empty_test(_) -> ok.



file_open_test(_Config) ->
  {ok, _File} = file:open("/etc/hosts", [read]),
  timer:sleep(300),
  Opts = #{some_pid => whereis(file_server_2)},
  ok.



% TODO: support expressions with record syntax
% transform them during parse_transform into tuple matching
ets_match_spec_transform(_) ->
  Tid = ets:new(test_table1, []),
  true = ets:insert(Tid, [{key1, <<"val1">>}]),
  MSpec1 = ets:fun2ms(fun ({Key, _Value} = E) when Key =:= key1 -> E end),
  [{key1, <<"val1">>}] = ets:select(Tid, MSpec1),
  ok.



% test record is defined in records.hrl
ets_mspec_with_records(_) ->
  Tid = ets:new(test_table2, []),
  Rec = #test_record{field2 = <<"foobar">>, field1 = 1},
  true = ets:insert(Tid, [Rec#test_record{field3 = some_value}]),
  MSpec = ets:fun2ms(fun (#test_record{field2 = <<"foobar">>, _ = '_'} = E) -> E end),
  [#test_record{field1 = 1, field2 = <<"foobar">>, field3 = Val}] = ets:select(Tid, MSpec),
  some_value = Val,
  ok.



open_port_and_change_owner(_) ->
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
  % Port ! {self(), {connect, Pid}},
  % receive {Port, connected} -> ok
  % after 1000 -> error(expected_to_change_owner)
  % end,
  Pid ! {self(), close_port},

  receive
    {Pid, done} -> ok
  after 1000 ->
    error(timeout_port_close)
  end.



port_dies_with_bad_reason(_) ->
  % load our own driver to ask it to fail
  % to demonstrate port that dies with reason
  % and how it displayed
  ok = erl_ddll:load_driver(code:priv_dir(batiscaph), "batiscaph_drv"),

  Pid = spawn(fun () ->
    receive start -> ok end,
    Port = erlang:open_port({spawn_driver, "batiscaph_drv"}, []),
    erlang:port_command(Port, <<"please_fail">>)
  end),

  MRef = erlang:monitor(process, Pid),
  Pid ! start,

  receive
    {'DOWN', MRef, process, Pid, Reason} ->
      ct:pal("process with port successfuly died with: ~p", [Reason])
  after 1000 -> error(timeout_awaiting_fail_from_port)
  end,
  ok.
