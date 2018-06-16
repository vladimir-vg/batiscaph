-module(batiscaph_delta_ct).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 ct:callback">>].

desired_attrs() ->
  [<<"callback">>].

init() ->
  #{suites => #{}}.



consume(#{<<"Type">> := <<"p1 ct:callback">>} = E, #{events := Suites} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  State;
  % #{<<"text">> := Text, <<"module">> := Module, <<"function">> := Function, <<"line">> := Line} = E,
  % Key = <<(erlang:integer_to_binary(At))/binary, " ", Pid/binary>>,
  % Event = #{
  %   <<"Pid">> => Pid, <<"At">> => At, <<"Id">> => Key, <<"text">> => Text,
  %   <<"module">> => Module, <<"function">> => Function, <<"line">> => Line
  % },
  % State#{events => Events#{Key => Event}};

consume(_E, State) ->
  State.



finalize(#{suites := Suites}) ->
  #{<<"ct_suites">> => Suites}.
