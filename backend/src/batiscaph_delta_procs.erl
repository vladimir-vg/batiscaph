-module(batiscaph_delta_procs).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:process trace start">>, <<"p2 erlang:process spawn">>, <<"p1 erlang:process exit">>].

desired_attrs() ->
  [].

init() ->
  #{procs => #{}}.



consume(#{<<"Type">> := <<"p1 erlang:process trace start">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  P = maps:get(Pid, Procs, #{<<"Pid">> => Pid, <<"Children">> => #{}}),
  P1 = P#{<<"TraceStartedAt">> => At},
  State#{procs => Procs#{Pid => P1}};

consume(#{<<"Type">> := <<"p2 erlang:process spawn">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := ParentPid, <<"Pid2">> := ChildPid} = E,
  P = #{<<"SpawnedAt">> => At, <<"Pid">> => ChildPid, <<"ParentPid">> => ParentPid, <<"Children">> => #{}},
  #{<<"Children">> := Children} = Parent = maps:get(ParentPid, Procs, #{<<"Pid">> => ParentPid, <<"Children">> => #{}}),
  Parent1 = Parent#{<<"Children">> => Children#{ChildPid => At}},
  State#{procs => Procs#{ChildPid => P, ParentPid => Parent1}};

consume(#{<<"Type">> := <<"p1 erlang:process exit">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  P = maps:get(Pid, Procs, #{<<"Pid">> => Pid, <<"Children">> => #{}}),
  P1 = P#{<<"ExitedAt">> => At},
  State#{procs => Procs#{Pid => P1}};

consume(_E, State) ->
  State.



finalize(#{procs := Procs}) ->
  #{<<"erlang-processes">> => Procs}.
