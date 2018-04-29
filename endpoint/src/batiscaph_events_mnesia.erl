-module(batiscaph_mnesia_events).
-export([create_tables/0, drop_tables/0]).
-export([insert/1]).

% different select queries
-export([
  select_instances_infos_with_ids/1,
  select_events/1,
  select_plug_request_info/1, select_cowboy_request_info/1,
  select_instances_ids/0
]).



% This module implements same database interface as batiscaph_clk_events
% but for Mnesia.
%
% Mnesia is used only to reduce number of dependencies for setting it up for the first time.



create_tables() ->
  ok.



drop_tables() ->
  ok.



insert(Events) ->
  ok.



select_instances_infos_with_ids([]) -> {ok, []};
select_instances_infos_with_ids(Ids) ->
  {ok, []}.



select_events(#{
  instance_id := Id, types := Types, attrs := Attrs,
  earlier_than := now, limit := Limit
}) ->
  {ok, []}.



select_plug_request_info(#{
  pid := Pid, instance_id := InstanceId,
  started_at := StartedAt, stopped_at := StoppedAt
}) ->
  {ok, undefined}.



select_cowboy_request_info(#{
  pid := Pid, instance_id := InstanceId,
  started_at := StartedAt, stopped_at := StoppedAt
}) ->
  {ok, undefined}.



select_instances_ids() ->
  {ok, []}.
