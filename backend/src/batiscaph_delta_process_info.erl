-module(batiscaph_delta_process_info).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:process process_info">>].

desired_attrs() ->
  process_info_attrs().

process_info_attrs() ->
  [
    <<"current_function">>,<<"dictionary">>,<<"initial_call">>,
    <<"links">>,<<"message_queue_len">>,<<"monitors">>,<<"registered_name">>
  ].



init() ->
  #{procs => #{}}.



consume(#{<<"Type">> := <<"p1 erlang:process process_info">>} = E, #{procs := Procs} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  #{<<"Changes">> := Changes} = P = maps:get(Pid, Procs, #{<<"Pid">> => Pid, <<"Changes">> => #{}}),
  Attrs = process_info_attrs(),
  Props = maps:filter(fun
    (_K, <<>>) -> false;
    (K, _V) -> lists:member(K, Attrs)
  end, E),
  P1 = P#{<<"Changes">> => Changes#{At => Props}},
  State#{procs => Procs#{Pid => P1}};

consume(_E, State) ->
  State.



finalize(#{procs := Procs}) ->
  #{<<"erlang-processes-info">> => Procs}.
