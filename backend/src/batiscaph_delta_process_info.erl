-module(batiscaph_delta_process_info).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:process process_info">>, <<"p1 erlang:process exit">>].

desired_attrs() ->
  process_info_attrs().

process_info_attrs() ->
  [
    <<"current_function">>,<<"dictionary">>,<<"initial_call">>,
    <<"links">>,<<"message_queue_len">>,<<"monitors">>,<<"registered_name">>,
    <<"reason">>
  ].



init() ->
  #{procs => #{}}.



consume(#{<<"Type">> := <<"p1 erlang:process process_info">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  New = #{<<"Pid">> => Pid, <<"Changes">> => #{}},
  P = maps:get(Pid, Procs, New),
  #{<<"Changes">> := Changes} = P,
  Attrs = process_info_attrs(),
  Props = maps:filter(fun
    (_K, <<>>) -> false;
    (K, _V) -> lists:member(K, Attrs)
  end, E),
  P1 = P#{<<"Changes">> => Changes#{At => Props}},
  State#{procs => Procs#{Pid => P1}};

consume(#{<<"Type">> := <<"p1 erlang:process exit">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := _, <<"Pid1">> := Pid, <<"reason">> := Reason} = E,
  New = #{<<"Pid">> => Pid, <<"Changes">> => #{}},
  P = maps:get(Pid, Procs, New),
  P1 = P#{<<"ExitReason">> => Reason},
  State#{procs => Procs#{Pid => P1}};

consume(_E, State) ->
  State.



finalize(#{procs := Procs}) ->
  #{<<"erlang_processes_info">> => Procs}.
