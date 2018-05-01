-module(batiscaph_delta_process_info_binaries).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:process process_info_binary">>].

desired_attrs() ->
  [<<"binaries">>].



init() ->
  #{procs => #{}}.



consume(#{<<"Type">> := <<"p1 erlang:process process_info_binary">>, <<"binaries">> := <<>>}, State) ->
  State;

consume(#{<<"Type">> := <<"p1 erlang:process process_info_binary">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := _At, <<"Pid1">> := Pid, <<"binaries">> := Binaries} = E,
  Binaries1 = erlang:binary_to_term(Binaries, [safe]),
  P = maps:get(Pid, Procs, #{}),
  P1 = maps:merge(P, Binaries1),
  State#{procs => Procs#{Pid => P1}};

consume(_E, State) ->
  State.



finalize(#{procs := Procs}) ->
  #{<<"erlang-processes-info-binaries">> => Procs}.
