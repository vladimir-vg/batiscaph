-module(graph_producer).
-behaviour(gen_server).
-export([delta_json/2]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(FETCH_AFTER, 1000).
-define(MAX_EVENTS_PER_FETCH, 1000).



%%% This process fetches fresh events from clickhouse,
%%% analyzes them, and constructs neo4j graph accordingly



-record(graph_producer, {
  id :: binary(),
  websocket_pid,
  fetch_timer,
  last_delta_at = 0 :: non_neg_integer(),
  last_checked_at :: undefined | {Secs :: non_neg_integer(), Mcs :: non_neg_integer()}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id, WebsocketPid) ->
  gen_server:start_link(?MODULE, [Id, WebsocketPid], []).

init([Id, WebsocketPid]) ->
  self() ! check_events,
  {ok, #graph_producer{id = Id, websocket_pid = WebsocketPid}}.



handle_info(new_events_stored, State) ->
  {ok, State1} = set_fetch_timer(State),
  {noreply, State1};

handle_info(check_events, State) ->
  {ok, State1} = clear_fetch_timer(State),
  {ok, State2} = fetch_and_process_events(State1),
  {ok, State3} = send_delta_to_websocket(State2),
  {noreply, State3};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



clear_fetch_timer(#graph_producer{fetch_timer = undefined} = State) ->
  {ok, State};

clear_fetch_timer(#graph_producer{fetch_timer = Timer} = State) ->
  erlang:cancel_timer(Timer),
  {ok, State#graph_producer{fetch_timer = undefined}}.



set_fetch_timer(#graph_producer{fetch_timer = undefined} = State) ->
  Timer = erlang:send_after(?FETCH_AFTER, self(), check_events),
  {ok, State#graph_producer{fetch_timer = Timer}};

% timer is already set, do nothing
set_fetch_timer(#graph_producer{fetch_timer = _} = State) ->
  {ok, State}.



fetch_and_process_events(#graph_producer{id = Id} = State) ->
  case fetch_events(State) of
    {more, Events, State1} ->
      self() ! check_events,
      ok = n4j_processes:update(Id, Events),
      {ok, State1};

    {ok, Events, State1} ->
      ok = n4j_processes:update(Id, Events),
      {ok, State1}
  end.

fetch_events(#graph_producer{last_checked_at = LastAt, id = Id} = State) ->
  Opts = #{instance_id => Id, limit => ?MAX_EVENTS_PER_FETCH, type_in => n4j_processes:desired_event_types()},
  Opts1 = case LastAt of
    undefined -> Opts;
    _ -> Opts#{'after' => LastAt}
  end,

  case clk_events:select(Opts1) of
    {ok, []} -> {ok, [], State};

    {ok, Events} when length(Events) == ?MAX_EVENTS_PER_FETCH ->
      #{<<"at_s">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {more, Events, State#graph_producer{last_checked_at = {At, Mcs}}};

    {ok, Events} ->
      #{<<"at_s">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {ok, Events, State#graph_producer{last_checked_at = {At, Mcs}}}
  end.



send_delta_to_websocket(#graph_producer{last_delta_at = LastAt, id = Id, websocket_pid = Pid} = State) ->
  {ok, Delta} = delta_json(Id, LastAt),
  {ok, LastAt1} = lastest_timestamp_in_delta(LastAt, Delta),

  Pid ! {delta, Delta},
  {ok, State#graph_producer{last_delta_at = LastAt1}}.



delta_json(Id, LastAt) ->
  ClkOpts = clickhouse_opts(Id, LastAt),
  {ok, TableEvents} = clk_events:select(ClkOpts),
  Neo4jOpts = neo4j_opts(Id, LastAt, TableEvents),
  {ok, #{processes := Processes, events := GraphEvents}} = n4j_processes:delta_json(Neo4jOpts),
  Delta = #{processes => Processes, graph_events => GraphEvents, table_events => TableEvents},
  {ok, Delta}.


table_events_types() ->
  [<<"shell_input">>,<<"shell_input_expected">>,<<"shell_output">>].

clickhouse_opts(Id, LastAt) ->
  #{instance_id => Id, 'after' => LastAt, type_in => table_events_types()}.



neo4j_opts(Id, LastAt, TableEvents) ->
  Opts0 = #{instance_id => Id, 'after' => LastAt},
  Pids = [P || #{<<"pid">> := P} <- TableEvents],
  case Pids of
    [] -> Opts0;
    [_|_] -> Opts0#{include_processes => Pids}
  end.



% lastest_timestamp_in_delta(undefined, Delta) ->
%   lastest_timestamp_in_delta(0, Delta);

% I know it's ugly, but works for now,
% should be replaced with something more simple and effective
lastest_timestamp_in_delta(LastAt, #{processes := Processes, table_events := TableEvents, graph_events := GraphEvents}) when is_integer(LastAt) ->
  LastAt1 = case lists:last([undefined | TableEvents]) of
    #{<<"at">> := At1} when At1 > LastAt -> At1;
    _ -> LastAt
  end,
  LastAt2 = case lists:last([undefined | GraphEvents]) of
    #{<<"at">> := At4} when At4 > LastAt1 -> At4;
    _ -> LastAt1
  end,
  ProcTimestamps = lists:map(fun (#{<<"events">> := Events1} = P) ->
    EventsTimestamps = [At || #{<<"at">> := At} <- Events1],
    AppearedAt = case maps:get(<<"appearedAt">>, P, null) of
      At2 when is_integer(At2) -> At2;
      null -> LastAt
    end,
    DisappearedAt = case maps:get(<<"disappearedAt">>, P, null) of
      At3 when is_integer(At3) -> At3;
      null -> LastAt
    end,
    lists:max([AppearedAt, DisappearedAt] ++ EventsTimestamps)
  end, Processes),
  lists:max([LastAt2] ++ ProcTimestamps).
