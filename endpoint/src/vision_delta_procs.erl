-module(vision_delta_procs).
-behaviour(vision_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:process trace start">>, <<"p2 erlang:process spawn">>, <<"p1 erlang:process exit">>].

desired_attrs() ->
  [].

init() ->
  #{procs => #{}}.



consume(#{<<"Type">> := <<"p1 erlang:process trace start">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  P = #{<<"TraceStartedAt">> => At, <<"Pid">> => Pid},
  State#{procs => Procs#{Pid => P}};

consume(#{<<"Type">> := <<"p2 erlang:process spawn">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := ParentPid, <<"Pid2">> := ChildPid} = E,
  P = #{<<"SpawnedAt">> => At, <<"Pid">> => ChildPid, <<"ParentPid">> => ParentPid},
  ParentP = maps:get(ParentPid, Procs, #{<<"Pid">> => ParentPid}), % make at least empty map, just to indicate that pid existed
  State#{procs => Procs#{ChildPid => P, ParentPid => ParentP}};

consume(#{<<"Type">> := <<"p1 erlang:process exit">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  P = maps:get(Pid, Procs, #{}),
  P1 = P#{<<"ExitedAt">> => At},
  State#{procs => Procs#{Pid => P1}};

consume(_E, State) ->
  State.



finalize(#{procs := Procs}) ->
  #{<<"erlang-processes">> => Procs}.
