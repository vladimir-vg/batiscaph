-module(vision_delta_cowboy).
-behaviour(vision_delta_producer).
-export([desired_types/0, desired_attrs/0, init/0, consume/2, finalize/1]). % delta producer callbacks
-export([parse_id/1]).



% this is a brother-module for vision_probe_feature_phoenix
% that takes events and produces delta



% this id is used only on client side and not stored anywere
% can be safely changed
format_id(Pid, StartedAt, StoppedAt) ->
  iolist_to_binary([Pid, <<"-">>, integer_to_binary(StartedAt), <<"-">>, integer_to_binary(StoppedAt)]).

parse_id(Id) ->
  [Pid, StartedAtBin, StoppedAtBin] = binary:split(Id, <<"-">>, [global]),
  #{pid => Pid,
    started_at => binary_to_integer(StartedAtBin),
    stopped_at => binary_to_integer(StoppedAtBin)
  }.



desired_types() ->
  [
    <<"p1 cowboy:request init start">>, <<"p1 cowboy:request init stop">>,
    <<"p1 cowboy:request handle start">>, <<"p1 cowboy:request handle stop">>,
    <<"p1 cowboy:request reply">>
  ].

desired_attrs() ->
  [<<"method">>, <<"path">>, <<"resp_code">>].

init() ->
  #{
    ongoing_reqs => #{}, % key is pid
    ready_reqs => #{} % key is some unique binary
  }.



consume(#{<<"Type">> := <<"p1 cowboy:request init start">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"method">> := Method, <<"path">> := Path} = E,
  R = #{
    <<"Pid">> => Pid, <<"Method">> => Method, <<"Path">> => Path,
    <<"init">> => #{<<"StartedAt">> => At}
  },
  State#{ongoing_reqs => Ongoing#{Pid => R}};

consume(#{<<"Type">> := <<"p1 cowboy:request init stop">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  R = maps:get(Pid, Ongoing),
  #{<<"init">> := Init} = R,
  R1 = R#{<<"init">> => Init#{<<"StoppedAt">> => At}},
  State#{ongoing_reqs => Ongoing#{Pid => R1}};

consume(#{<<"Type">> := <<"p1 cowboy:request handle start">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  R = maps:get(Pid, Ongoing),
  R1 = R#{<<"handle">> => #{<<"StartedAt">> => At}},
  State#{ongoing_reqs => Ongoing#{Pid => R1}};

consume(#{<<"Type">> := <<"p1 cowboy:request reply">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{ <<"Pid1">> := Pid, <<"resp_code">> := Code} = E,
  R = maps:get(Pid, Ongoing),
  R1 = R#{<<"RespCode">> => Code},
  State#{ongoing_reqs => Ongoing#{Pid => R1}};

consume(#{<<"Type">> := <<"p1 cowboy:request handle stop">>} = E, #{ongoing_reqs := Ongoing, ready_reqs := Ready} = State) ->
  #{<<"At">> := StoppedAt, <<"Pid1">> := Pid} = E,
  {R, Ongoing1} = maps:take(Pid, Ongoing),
  #{<<"init">> := #{<<"StartedAt">> := StartedAt}, <<"handle">> := Handle} = R,
  Id = format_id(Pid, StartedAt, StoppedAt),
  R1 = R#{<<"handle">> => Handle#{<<"StoppedAt">> => StoppedAt}, <<"Id">> => Id},
  State#{ongoing_reqs => Ongoing1, ready_reqs => Ready#{Id => R1}};

consume(_E, State) ->
  State.



finalize(#{ready_reqs := Reqs}) ->
  #{<<"cowboy:requests">> => Reqs}.
