-module(vision_delta_phoenix).
-behaviour(vision_delta_producer).
-export([desired_types/0, init/0, consume/2, finalize/1]).



% this is a brother-module for vision_probe_feature_phoenix
% that takes events and produces delta



% TODO: it's not clear how to produce correct delta
% when start and stop of request are in different chunks
% but we can't give good id from single event
% because request_id appears only at second event
% and pid may be reused by other requests.
% There must be some kind of continuation, in deltas.
% Or just store incomplete requests separately, where their pids gonna be unique?
%
% merge callback for chunks?



desired_types() ->
  [<<"p1 plug:request start">>, <<"p1 plug:request stop">>].

init() ->
  #{
    ongoing_reqs => #{}, % key is pid
    ready_reqs => #{} % key is some unique binary
  }.



consume(#{<<"Type">> := <<"p1 plug:request start">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  R = #{<<"StartedAt">> => At, <<"Pid">> => Pid},
  State#{ongoing_reqs => Ongoing#{Pid => R}};

consume(#{<<"Type">> := <<"p1 plug:request stop">>} = E, #{ongoing_reqs := Ongoing, ready_reqs := Ready} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid} = E,
  {R, Ongoing1} = maps:take(Pid, Ongoing),
  R1 = R#{<<"StoppedAt">> => At},
  Key = iolist_to_binary([Pid, <<"-">>, integer_to_binary(At)]), % for simplicity
  State#{ongoing_reqs => Ongoing1, ready_reqs => Ready#{Key => R1}};

consume(_E, State) ->
  State.



finalize(#{ready_reqs := Reqs}) ->
  #{<<"plug:requests">> => Reqs}.
