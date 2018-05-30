-module(batiscaph_delta_lager).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 lager:event">>].

desired_attrs() ->
  [<<"text">>, <<"module">>, <<"function">>, <<"line">>].

init() ->
  #{events => #{}}.



consume(#{<<"Type">> := <<"p1 lager:event">>} = E, #{events := Events} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  #{<<"text">> := Text, <<"module">> := Module, <<"function">> := Function, <<"line">> := Line} = E,
  Key = <<(erlang:integer_to_binary(At))/binary, " ", Pid/binary>>,
  Event = #{
    <<"Pid">> => Pid, <<"At">> => At, <<"Id">> => Key, <<"text">> => Text,
    <<"module">> => Module, <<"function">> => Function, <<"line">> => Line
  },
  State#{events => Events#{Key => Event}};

consume(_E, State) ->
  State.



finalize(#{events := Events}) ->
  #{<<"lager_events">> => Events}.
