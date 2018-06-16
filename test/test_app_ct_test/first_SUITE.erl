-module(first_SUITE).
-export([all/0, groups/0]).
-export([testcase1/1, must_fail1/1]).



all() ->
  [testcase1, {group, group1}, must_fail1].

groups() ->
  [
    {group1, [parallel], [
      {group, group2},
      {group, group3},
      testcase1,
      must_fail1
    ]},
    {group2, [shuffle], [
      {group, group3},
      must_fail1,
      testcase1
    ]},
    {group3, [], [
      testcase1,
      must_fail1
    ]}
  ].

testcase1(_Config) ->
  ok.

must_fail1(_Config) ->
  error(reason_of_failure),
  ok.
