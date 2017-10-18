-module(showcases_SUITE).
-export([all/0, groups/0]).
-export([file_open_test/1, ets_match_spec_transform/1]).
-batiscaph_steps([file_open_test, ets_match_spec_transform]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("records.hrl").
-compile({parse_transform, batiscaph_suite_transform}).



%%% This test suite tries to play out different code examples
%%% records JSON results of delta, makes it available for display.
%%%
%%% These tests useful to check that different entities displayed correctly
%%% in different edge cases.



all() ->
  [ets_match_spec_transform, {group, group1}, file_open_test, {group, group2}].

groups() ->
  [
    {group1, [parallel], [
      {group, group1_nested},
      file_open_test
    ]},
    {group2, [], [file_open_test]},
    {group1_nested, [], [file_open_test]}
  ].



file_open_test(_Config) ->
  {ok, _File} = file:open("/etc/hosts", [read]),
  ok.



ets_match_spec_transform(_) ->
  Tid = ets:new(test_table1, []),
  true = ets:insert(Tid, [{key1, <<"val1">>}]),
  % true = ets:insert(Tid, [#test_record{field2 = <<"foobar">>, field1 = 1}]),
  MSpec1 = ets:fun2ms(fun ({Key, _Value} = E) when Key =:= key1 -> E end),
  [{key1, <<"val1">>}] = ets:select(Tid, MSpec1),
  % MSpec2 = ets:fun2ms(fun (#test_record{field2 = <<"foobar">>, _ = '_'} = E) -> E end),
  % [#test_record{field1 = 1, field2 = <<"foobar">>}] = ets:select(Tid, MSpec2),
  ok.
