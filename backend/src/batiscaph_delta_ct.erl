-module(batiscaph_delta_ct).
-behaviour(batiscaph_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks



desired_types() ->
  [
    <<"p1 ct:pre_init_per_suite">>,
    <<"p1 ct:post_init_per_suite">>,
    <<"p1 ct:pre_end_per_suite">>,
    <<"p1 ct:post_end_per_suite">>,
    <<"p1 ct:pre_init_per_group">>,
    <<"p1 ct:post_init_per_group">>,
    <<"p1 ct:pre_end_per_group">>,
    <<"p1 ct:post_end_per_group">>,
    <<"p1 ct:pre_init_per_testcase">>,
    <<"p1 ct:post_init_per_testcase">>,
    <<"p1 ct:pre_end_per_testcase">>,
    <<"p1 ct:post_end_per_testcase">>
  ].

desired_attrs() ->
  [<<"suite">>, <<"groups">>, <<"testcase">>].

init() ->
  #{callbacks => #{}}.



consume(#{<<"Type">> := <<"p1 ct:pre_init_per_suite">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite} = E,
  Key = <<Suite/binary, " init_per_suite">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_init_per_suite">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite} = E,
  Key = <<Suite/binary, " init_per_suite">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:pre_end_per_suite">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite} = E,
  Key = <<Suite/binary, " end_per_suite">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_end_per_suite">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite} = E,
  Key = <<Suite/binary, " end_per_suite">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};



consume(#{<<"Type">> := <<"p1 ct:pre_init_per_group">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups} = E,
  Key = <<Suite/binary, " ", Groups/binary, " init_per_group">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_init_per_group">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups} = E,
  Key = <<Suite/binary, " ", Groups/binary, " init_per_group">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:pre_end_per_group">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups} = E,
  Key = <<Suite/binary, " ", Groups/binary, " end_per_group">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_end_per_group">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups} = E,
  Key = <<Suite/binary, " ", Groups/binary, " end_per_group">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};



consume(#{<<"Type">> := <<"p1 ct:pre_init_per_testcase">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups, <<"testcase">> := TC} = E,
  Groups1 = case Groups of
    <<>> -> <<>>;
    _ -> <<" ", Groups/binary>>
  end,
  Key = <<Suite/binary, Groups1/binary, " ", TC/binary, " init_per_testcase">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_init_per_testcase">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups, <<"testcase">> := TC} = E,
  Groups1 = case Groups of
    <<>> -> <<>>;
    _ -> <<" ", Groups/binary>>
  end,
  Key = <<Suite/binary, Groups1/binary, " ", TC/binary, " init_per_testcase">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:pre_end_per_testcase">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups, <<"testcase">> := TC} = E,
  Groups1 = case Groups of
    <<>> -> <<>>;
    _ -> <<" ", Groups/binary>>
  end,
  Key = <<Suite/binary, Groups1/binary, " ", TC/binary,  " end_per_testcase">>,
  Callbacks1 = Callbacks#{Key => #{<<"StartedAt">> => At, <<"Pid">> => Pid}},
  State#{callbacks => Callbacks1};

consume(#{<<"Type">> := <<"p1 ct:post_end_per_testcase">>} = E, #{callbacks := Callbacks} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"suite">> := Suite, <<"groups">> := Groups, <<"testcase">> := TC} = E,
  Groups1 = case Groups of
    <<>> -> <<>>;
    _ -> <<" ", Groups/binary>>
  end,
  Key = <<Suite/binary, Groups1/binary, " ", TC/binary, " end_per_testcase">>,
  Callback = #{<<"Pid">> := Pid} = maps:get(Key, Callbacks),
  Callbacks1 = Callbacks#{Key => Callback#{<<"StoppedAt">> => At}},
  State#{callbacks => Callbacks1};



consume(_E, State) ->
  State.



finalize(#{callbacks := Callbacks}) ->
  #{<<"ct_callbacks">> => Callbacks}.
