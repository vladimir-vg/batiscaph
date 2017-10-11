-module(showcases_SUITE).
-export([all/0]).
-export([file_open_test/1, ets_match_spec_transform/1]).
-espace_steps([file_open_test, ets_match_spec_transform]).
-include_lib("stdlib/include/ms_transform.hrl").
-compile({parse_transform, ct_espace_steps_transform}).



%%% This test suite tries to play out different code examples
%%% records JSON results of delta, makes it available for display.
%%%
%%% These tests useful to check that different entities displayed correctly
%%% in different edge cases.



all() ->
  [file_open_test, ets_match_spec_transform].



file_open_test(_Config) ->
  {ok, _File} = file:open("/etc/hosts", [read]),
  ok.



ets_match_spec_transform(_) ->
  Tid = ets:new(test_table1, []),
  true = ets:insert(Tid, [{key1, <<"val1">>}]),
  MSpec = ets:fun2ms(fun ({Key, _Value} = E) when Key =:= key1 -> E end),
  [{key1, <<"val1">>}] = ets:select(Tid, MSpec),
  ok.
