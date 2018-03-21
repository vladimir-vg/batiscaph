-module(vision_delta_plug).
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
  [<<"p1 plug:request start">>, <<"p1 plug:request stop">>]. % , <<"p1 plug:plug stop">>, <<"p1 plug:plug start">>

desired_attrs() ->
  [<<"method">>, <<"path">>, <<"resp_code">>]. % , <<"module">>

init() ->
  #{
    ongoing_reqs => #{}, % key is pid
    ready_reqs => #{} % key is some unique binary
  }.



consume(#{<<"Type">> := <<"p1 plug:request start">>} = E, #{ongoing_reqs := Ongoing} = State) ->
  #{<<"At">> := At, <<"Pid1">> := Pid, <<"method">> := Method, <<"path">> := Path} = E,
  R = #{<<"StartedAt">> => At, <<"Pid">> => Pid, <<"method">> => Method, <<"path">> => Path},
  State#{ongoing_reqs => Ongoing#{Pid => R}};

consume(#{<<"Type">> := <<"p1 plug:request stop">>} = E, #{ongoing_reqs := Ongoing, ready_reqs := Ready} = State) ->
  #{<<"At">> := StoppedAt, <<"Pid1">> := Pid, <<"resp_code">> := Code} = E,
  {R, Ongoing1} = maps:take(Pid, Ongoing),

  #{<<"StartedAt">> := StartedAt} = R,
  Id = format_id(Pid, StartedAt, StoppedAt),

  R1 = R#{<<"StoppedAt">> => StoppedAt, <<"resp_code">> => Code, <<"id">> => Id},
  % R2 = case maps:get(plugs, R1, undefined) of
  %   undefined -> R1;
  %   Plugs -> R1#{plugs => lists:reverse(Plugs)}
  % end,
  State#{ongoing_reqs => Ongoing1, ready_reqs => Ready#{Id => R1}};

% consume(#{<<"Type">> := <<"p1 plug:plug start">>} = E, #{ongoing_reqs := Ongoing} = State) ->
%   #{<<"At">> := At, <<"Pid1">> := Pid, <<"module">> := Module} = E,
%   R = maps:get(Pid, Ongoing),
%   PlugsStack = maps:get(current_plugs_stack, R, []),
%   P = #{<<"StartedAt">> => At, <<"module">> => Module},
%   PlugsStack1 = [P | PlugsStack],
%   R1 = R#{current_plugs_stack => PlugsStack1},
%   Ongoing1 = Ongoing#{Pid => R1},
%   State#{ongoing_reqs => Ongoing1};
% 
% consume(#{<<"Type">> := <<"p1 plug:plug stop">>} = E, #{ongoing_reqs := Ongoing} = State) ->
%   #{<<"At">> := At, <<"Pid1">> := Pid, <<"module">> := Module} = E,
%   R = maps:get(Pid, Ongoing),
%   [#{<<"module">> := Module} = P | PlugsStack] = maps:get(current_plugs_stack, R),
%   P1 = P#{<<"StoppedAt">> => At},
%   P2 = case maps:get(plugs, P1, undefined) of
%     undefined -> P1;
%     NestedPlugs -> P1#{plugs => lists:reverse(NestedPlugs)}
%   end,
% 
%   case PlugsStack of
%     [] ->
%       ReqPlugs = maps:get(plugs, R, []),
%       R1 = R#{plugs => [P2 | ReqPlugs], current_plugs_stack => []},
%       State#{ongoing_reqs => Ongoing#{Pid => R1}};
% 
%     [ParentPlug | Rest] ->
%       SiblingsPlugs = maps:get(plugs, ParentPlug, []),
%       ParentPlug1 = ParentPlug#{plugs => [P2 | SiblingsPlugs]},
%       R1 = R#{current_plugs_stack => [ParentPlug1 | Rest]},
%       State#{ongoing_reqs => Ongoing#{Pid => R1}}
%   end;

consume(_E, State) ->
  State.



finalize(#{ready_reqs := Reqs}) ->
  #{<<"plug:requests">> => Reqs}.