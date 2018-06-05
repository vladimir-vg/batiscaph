-module(empty_SUITE).
-export([all/0]).
-export([testcase1/1]).



all() ->
  [testcase1].

testcase1(_Config) ->
  ok.
