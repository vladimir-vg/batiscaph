-module(showcases_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([file_open_test/1, ets_match_spec_transform/1, open_port_and_change_owner/1]).
-batiscaph_steps([file_open_test, ets_match_spec_transform, open_port_and_change_owner]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("records.hrl").
-compile({parse_transform, batiscaph_suite_transform}).



%%% This test suite should be used only for display tests.
%%% Testing application logic should be done in other suites.
%%% Need to include all possible edge cases of displayed data.
%%% Would be great to test display of incorrect deltas (should somehow indicate that in UI).



init_per_suite(Config) ->
  BatiscaphNode = list_to_atom("batiscaph@" ++ net_adm:localhost()),
  application:set_env(batiscaph, batiscaph_node, BatiscaphNode),
  case net_adm:ping(BatiscaphNode) of
    pong -> Config;
    pang -> {skip, {unable_to_connect_to_batiscaph_node, BatiscaphNode}}
  end.

end_per_suite(Config) ->
  % collector consumed all trace messages
  ok = gen_server:call(z__client_collector, flush),

  exit(whereis(z__client_sup), kill),
  Config.



all() ->
  [{group, main}, {group, group2}].

groups() ->
  [
    {main, [parallel], [
      file_open_test,
      ets_match_spec_transform,
      open_port_and_change_owner
    ]},
    {group2, [parallel], [file_open_test, ets_match_spec_transform, {group, group2_nested}]},
    {group2_nested, [], [file_open_test, ets_match_spec_transform]}
  ].



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
  % true = ets:insert(Tid, [#test_record{field2 = <<"foobar">>, field1 = 1}]),
  MSpec1 = ets:fun2ms(fun ({Key, _Value} = E) when Key =:= key1 -> E end),
  [{key1, <<"val1">>}] = ets:select(Tid, MSpec1),
  % MSpec2 = ets:fun2ms(fun (#test_record{field2 = <<"foobar">>, _ = '_'} = E) -> E end),
  % [#test_record{field1 = 1, field2 = <<"foobar">>}] = ets:select(Tid, MSpec2),
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