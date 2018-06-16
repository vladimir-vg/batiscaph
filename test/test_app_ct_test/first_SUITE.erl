-module(first_SUITE).
-export([all/0, groups/0]).
-export([testcase1/1]).



all() ->
  [testcase1, {group, group1}].

groups() ->
  [
    {group1, [parallel], [
      {group, group2},
      {group, group3},
      testcase1
    ]},
    {group2, [shuffle], [
      {group, group3},
      testcase1
    ]},
    {group3, [], [
      testcase1
    ]}
  ].

testcase1(_Config) ->
  ok.
