-module(batiscaph_delta_shell).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [<<"p1 erlang:shell output">>, <<"p1 erlang:shell input">>].

desired_attrs() ->
  [<<"output">>, <<"input">>, <<"prompt">>].

init() ->
  #{commands => #{}, current_command => #{<<"Outputs">> => #{}}}.



consume(#{<<"Type">> := <<"p1 erlang:shell output">>} = E, #{current_command := Cmd} = State) ->
  #{<<"Outputs">> := Outputs} = Cmd,
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"output">> := Text} = E,
  C = #{<<"At">> => At, <<"Pid">> => Pid, <<"Text">> => Text},
  Id = iolist_to_binary([integer_to_binary(At), "-", Pid]),
  Cmd1 = Cmd#{<<"Outputs">> => Outputs#{Id => C}},
  State#{current_command => Cmd1};

consume(#{<<"Type">> := <<"p1 erlang:shell input">>} = E, State) ->
  {ok, #{current_command := Cmd} = State1} = save_current_command(State),
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"input">> := Text, <<"prompt">> := Prompt} = E,
  Cmd1 = Cmd#{<<"At">> => At, <<"Pid">> => Pid, <<"Input">> => Text, <<"Prompt">> => Prompt},
  State1#{current_command => Cmd1};

consume(_E, State) ->
  State.



save_current_command(#{commands := Cmds, current_command := Cmd} = State) ->
  Id = case maps:get(<<"At">>, Cmd, undefined) of
    undefined -> <<"0">>;
    At1 when is_integer(At1) ->
      iolist_to_binary([integer_to_binary(At1), "-", maps:get(<<"Pid">>, Cmd)])
  end,
  State1 = State#{commands => Cmds#{Id => Cmd}, current_command => #{<<"Outputs">> => #{}}},
  {ok, State1}.



finalize(State) ->
  {ok, #{commands := Cmds}} = save_current_command(State),
  #{<<"shell-commands">> => Cmds}.
