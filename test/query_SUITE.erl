-module(query_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  related_contexts/1,
  subcontext/1,
  context_related_pids/1
  % explicit_pids/1
]).



all() ->
  [
    related_contexts,
    subcontext,
    context_related_pids
    % explicit_pids
  ].



events({context, Context}, AtS, Id) ->
  Pid = bt:g(pid),
  Lines = erlang:term_to_binary([[1, "ok"]]),
  Events = [
    #{at_mcs => 0, type => <<"context_start">>, lines => Lines},

    #{at_mcs => 5, type => <<"var_mention">>, term => <<"maps:get(Config, foo)">>, pid1 => bt:g(pid), line => 1},
    #{at_mcs => 10, type => <<"expr_eval_start">>, term => <<"ok">>, line => 1},
    #{at_mcs => 20, type => <<"expr_eval_stop">>, term => <<"ok">>, line => 1, result => <<"ok">>},

    #{at_mcs => 1000, type => <<"context_stop">>}
  ],
  [E#{at_s => AtS, pid => Pid, context => Context, instance_id => Id} || E <- Events].



feed_events(Id) ->
  AtS = bt:g(at_s),

  Events1 = events({context, <<"context1">>}, AtS, Id),
  Events2 = events({context, <<"context1 subcontext1">>}, AtS, Id),
  Events3 = events({context, <<"context2">>}, AtS, Id),

  Events0 = Events1 ++ Events2 ++ Events3,

  Events = lists:sort(fun (A, B) ->
    maps:get(at_mcs, A) < maps:get(at_mcs, B)
  end, Events0),

  {ok, Pid} = remote_ctl:start_link(Id, #{node => false}),
  ok = gen_server:call(Pid, {events, bt:binarify_events(Events)}),

  {ok, Pid}.



init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),

  InstanceId = bt:g(instance_id, query_SUITE),
  {ok, Pid} = feed_events(InstanceId),
  unlink(Pid), % gonna be terminated in end_per_suite

  [{instance_id, InstanceId}, {remote_ctl_pid, Pid} | Config].



end_per_suite(Config) ->
  Pid = proplists:get_value(remote_ctl_pid, Config),
  exit(Pid, kill),
  undefined = erlang:process_info(Pid),
  Config.



 related_contexts(Config) ->
  Id = proplists:get_value(instance_id, Config),

  {ok, Delta1} = remote_ctl:delta_json(#{instance_id => Id, context => <<"context1">>}),
  #{contexts := Contexts1} = bt:atomize_delta(Delta1),

  % shouldn't contain unrelated contexts
  #{<<"context1">> := _, <<"context1 subcontext1">> := _} = Contexts1,
  0 = maps:size(maps:without([<<"context1">>, <<"context1 subcontext1">>], Contexts1)),

  {ok, Delta2} = remote_ctl:delta_json(#{instance_id => Id, context => <<"context2">>}),
  #{contexts := Contexts2, events := Events} = bt:atomize_delta(Delta2),

  #{<<"context2">> := _} = Contexts2,
  0 = maps:size(maps:without([<<"context2">>], Contexts2)),

  % expected to have two VAR_MENTION events, for parent and child contexts
  MentionEvents = [E || #{type := <<"VAR_MENTION">>} = E <- Events],
  2 = length(MentionEvents),

  ok.



subcontext(Config) ->
  Id = proplists:get_value(instance_id, Config),

  {ok, Delta} = remote_ctl:delta_json(#{instance_id => Id, context => <<"context1 subcontext1">>}),
  #{contexts := Contexts} = bt:atomize_delta(Delta),

  % shouldn't contain unrelated contexts
  #{<<"context1 subcontext1">> := _} = Contexts,
  0 = maps:size(maps:without([<<"context1 subcontext1">>], Contexts)),

  % expected to have two VAR_MENTION events, for parent and child contexts
  MentionEvents = [E || #{type := <<"VAR_MENTION">>} = E <- Events],
  1 = length(MentionEvents),

  ok.



context_related_pids(Config) ->
  Id = proplists:get_value(instance_id, Config),

  {ok, Delta} = remote_ctl:delta_json(#{instance_id => Id, context => <<"context1">>}),
  #{processes := Processes, events := Events} = bt:atomize_delta(Delta),

  Pids = lists:usort(lists:flatten([[Pid1, Pid2] || #{type := <<"VAR_MENTION">>, pid1 := Pid1, pid2 := Pid2} <- Events])),

  PidsCount = length(Pids),
  % we expect to have at least 4 four processes:
  % one context process and one mentioned
  % for both parent context and subcontext
  true = PidsCount >= 4,

  % exactly same count of pids is presented in processes delta
  PidsCount = maps:size(maps:with(Pids, Processes)),

  % none of extra are provided
  0 = maps:size(maps:without(Pids, Processes)),

  % expected to have two VAR_MENTION events, for parent and child contexts
  MentionEvents = [E || #{type := <<"VAR_MENTION">>} = E <- Events],
  1 = length(MentionEvents),

  ok.



% explicit_pids(Config) ->
%   Id = proplists:get_value(instance_id, Config),
% 
%   ok.