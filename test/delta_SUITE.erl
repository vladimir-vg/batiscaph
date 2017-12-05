-module(delta_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([context_events_create_context/1, mixed_expr_eval_events/1]).



% This suite start batiscaph, sends list of events
% and checks that resulting delta is correct



all() ->
  [{group, main}].

groups() ->
  [{main, [parallel], [
    % events_create_proceses,
    context_events_create_context,
    mixed_expr_eval_events
  ]}].



init_per_suite(Config) ->
  % do expect that all necessary ENV variables were specified in advance
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),
  Config.

end_per_suite(Config) ->
  Config.



%%% TODO:
%%% give link to neo4j url with query that selects problem graph
%%% also print raw events from clickhouse



context_events_create_context(_Config) ->
  Lines = [
    [15, <<"T1 = erlang:system_time(micro_seconds),">>],
    [16, <<"{ok, SomePid} = some_module:compicated_function(),">>],
    [17, <<"Duration = T1 - erlang:system_time(micro_seconds),">>],
    [18, <<"io:format(\"elapsed time: ~p~n\", [Duration]).">>]
  ],
  SomePid = bt:g(pid),
  Delta = produce_delta(with_map(#{at_s => bt:g(at_s), pid => bt:g(pid), context => <<"context1">>, instance_id => bt:g(instance_id, ?FUNCTION_NAME)}, [
    #{at_mcs => 1, type => <<"context_start">>, lines => erlang:term_to_binary(Lines)},
    #{at_mcs => 2, type => <<"expr_eval_start">>, term => <<"AST1">>, line => 15},
    #{at_mcs => 3, type => <<"expr_eval_stop">>, term => <<"AST1">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 4, type => <<"expr_eval_start">>, term => <<"AST2">>, line => 15},
    #{at_mcs => 5, type => <<"expr_eval_stop">>, term => <<"AST2">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 10, type => <<"var_bind">>, atom => <<"T1">>, term => <<"123456789">>},
    #{at_mcs => 12, type => <<"var_bind">>, atom => <<"SomePid">>, term => SomePid},
    #{at_mcs => 12, type => <<"var_mention">>, term => <<"SomePid">>, pid1 => SomePid},
    #{at_mcs => 510, type => <<"var_bind">>, atom => <<"Duration">>, term => <<"500">>},
    #{at_mcs => 1000, type => <<"context_stop">>}
  ])),

  #{contexts := #{context1 := Context}, events := Events} = Delta,
  #{context := <<"context1">>, pid := _, startedAt := At1, stoppedAt := At2, lines := Lines, variables := Vars, evals := Evals} = Context,
  true = At1 < At2,
  #{'T1' := <<"123456789">>, 'Duration' := <<"500">>, 'SomePid' := SomePid} = Vars,
  [#{pid1 := _, pid2 := SomePid, expr := <<"SomePid">>}] = [E || #{type := <<"VAR_MENTION">>} = E <- Events],
  #{'15' := #{exprs := [#{startedAt := _, stoppedAt := _, result := _}, #{startedAt := _, stoppedAt := _, result := _}]}} = Evals,
  ok.



% two different contexts were launched almost at the same time
% as a result, events got interleaved
% check that delta calculated correctly
mixed_expr_eval_events(_Config) ->
  Lines = [
    [15, <<"T1 = erlang:system_time(micro_seconds),">>],
    [16, <<"{ok, SomePid} = some_module:compicated_function(),">>],
    [17, <<"Duration = T1 - erlang:system_time(micro_seconds),">>],
    [18, <<"io:format(\"elapsed time: ~p~n\", [Duration]).">>]
  ],
  SomePid = bt:g(pid),
  AtS = bt:g(at_s),
  Events1 = with_map(#{at_s => AtS, pid => bt:g(pid), context => <<"mixed1">>, instance_id => bt:g(instance_id, ?FUNCTION_NAME)}, [
    #{at_mcs => 1, type => <<"context_start">>, lines => erlang:term_to_binary(Lines)},
    #{at_mcs => 2, type => <<"expr_eval_start">>, term => <<"AST1">>, line => 15},
    #{at_mcs => 3, type => <<"expr_eval_stop">>, term => <<"AST1">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 4, type => <<"expr_eval_start">>, term => <<"AST2">>, line => 15},
    #{at_mcs => 5, type => <<"expr_eval_stop">>, term => <<"AST2">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 10, type => <<"var_bind">>, atom => <<"T1">>, term => <<"123456789">>},
    #{at_mcs => 12, type => <<"var_bind">>, atom => <<"SomePid">>, term => SomePid},
    #{at_mcs => 12, type => <<"var_mention">>, term => <<"SomePid">>, pid1 => SomePid},
    #{at_mcs => 510, type => <<"var_bind">>, atom => <<"Duration">>, term => <<"500">>},
    #{at_mcs => 1000, type => <<"context_stop">>}
  ]),

  Events2 = with_map(#{at_s => bt:g(at_s), pid => bt:g(pid), context => <<"mixed2">>, instance_id => bt:g(instance_id, ?FUNCTION_NAME)}, [
    #{at_mcs => 1, type => <<"context_start">>, lines => erlang:term_to_binary(Lines)},
    #{at_mcs => 2, type => <<"expr_eval_start">>, term => <<"AST1">>, line => 15},
    #{at_mcs => 3, type => <<"expr_eval_stop">>, term => <<"AST1">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 4, type => <<"expr_eval_start">>, term => <<"AST2">>, line => 15},
    #{at_mcs => 5, type => <<"expr_eval_stop">>, term => <<"AST2">>, line => 15, result => <<"Term describing result of execution">>},
    #{at_mcs => 10, type => <<"var_bind">>, atom => <<"T1">>, term => <<"123456789">>},
    #{at_mcs => 12, type => <<"var_bind">>, atom => <<"SomePid">>, term => SomePid},
    #{at_mcs => 12, type => <<"var_mention">>, term => <<"SomePid">>, pid1 => SomePid},
    #{at_mcs => 510, type => <<"var_bind">>, atom => <<"Duration">>, term => <<"500">>},
    #{at_mcs => 1000, type => <<"context_stop">>}
  ]),

  % interleave these events together
  Events3 = lists:flatten([[E1, E2] || {E1, E2} <- lists:zip(Events1, Events2)]),
  #{contexts := #{mixed1 := Context1, mixed2 := Context2}, events := _} = produce_delta(Events3),
  #{context := <<"mixed1">>, pid := _, startedAt := _, stoppedAt := _, lines := Lines, variables := Vars, evals := _} = Context1,
  #{context := <<"mixed2">>, pid := _, startedAt := _, stoppedAt := _, lines := Lines, variables := Vars, evals := _} = Context2,
  #{'T1' := <<"123456789">>, 'Duration' := <<"500">>, 'SomePid' := SomePid} = Vars,

  ok.



%%% helper functions



produce_delta([#{instance_id := Id} | _] = Events) ->
  Events1 = bt:binarify_events(Events),
  {ok, Pid} = remote_ctl:start_link(Id, #{node => false}),
  ok = gen_server:call(Pid, {events, Events1}),
  {ok, Delta} = remote_ctl:delta_json(#{instance_id => Id}),
  bt:atomize_delta(Delta).



% same list of maps, but with merged pairs from first argument
with_map(Map, List) ->
  [maps:merge(Map, E) || E <- List].
