-module(graph_producer).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(FETCH_AFTER, 1000).
-define(MAX_EVENTS_PER_FETCH, 1000).



%%% This process fetches fresh events from clickhouse,
%%% analyzes them, and constructs neo4j graph accordingly



-record(graph_producer, {
  id :: binary(),
  fetch_timer,
  last_checked_at :: undefined | {Secs :: non_neg_integer(), Mcs :: non_neg_integer()}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
  self() ! check_events,
  {ok, #graph_producer{id = Id}}.



handle_info(new_events_stored, State) ->
  {ok, State1} = set_fetch_timer(State),
  {noreply, State1};

handle_info(check_events, State) ->
  {ok, State1} = clear_fetch_timer(State),
  {ok, State2} = fetch_and_process_events(State1),
  {noreply, State2};

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



% event types that are interesting for graph builder
event_types() ->
  [<<"spawn">>, <<"exit">>, <<"link">>, <<"unlink">>, <<"register">>, <<"unregister">>].



fetch_and_process_events(#graph_producer{id = Id} = State) ->
  case fetch_events(State) of
    {more, Events, State1} ->
      self() ! check_events,
      ok = process_events(Id, Events),
      {ok, State1};

    {ok, Events, State1} ->
      ok = process_events(Id, Events),
      {ok, State1}
  end.

fetch_events(#graph_producer{last_checked_at = LastAt, id = Id} = State) ->
  Opts = #{instance_id => Id, limit => ?MAX_EVENTS_PER_FETCH, type_in => event_types()},
  Opts1 = case LastAt of
    undefined -> Opts;
    _ -> Opts#{'after' => LastAt}
  end,

  case es_events:select(Opts1) of
    {ok, []} -> {ok, [], State};

    {ok, Events} when length(Events) == ?MAX_EVENTS_PER_FETCH ->
      #{<<"at">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {more, Events, State#graph_producer{last_checked_at = {At, Mcs}}};

    {ok, Events} ->
      #{<<"at">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {ok, Events, State#graph_producer{last_checked_at = {At, Mcs}}}
  end.



process_events(Id, Events) ->
  Statements = process_events(Id, Events, []),
  case Statements of
    [] -> ok;
    _ -> {ok, _} = neo4j:commit(Statements), ok
  end.

process_events(_Id, [], Acc) -> lists:flatten(lists:reverse(Acc));



process_events(Id, [#{<<"type">> := <<"spawn">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid1">> := Parent, <<"pid">> := Pid} = E,
  Statements = [
    % create parent process if not existed before
    { "MERGE (parent:Process { pid: {parent}, instance_id: {id} })\n"
      "ON CREATE SET parent.first_mentioned_at = {at}, parent.first_mentioned_at_mcs = {at_mcs}\n"
    , #{id => Id, parent => Parent, at => At, at_mcs => Mcs} },

    % create new process
    { "MATCH (parent:Process)\n"
      "WHERE parent.instance_id = {id} AND parent.pid = {parent}\n"
      "CREATE\t(proc:Process { instance_id: {id}, pid: {pid}, spawned_at: {at}, spawned_at_mcs: {at_mcs} }),\n"
      "\t(parent)-[:SPAWN { at: {at}, at_mcs: {at_mcs} }]->(proc)\n"
    , #{id => Id, parent => Parent, at => At, at_mcs => Mcs, pid => Pid} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"exit">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.exited_at = {at}, proc.exited_at_mcs = {at_mcs}, proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
      "ON MATCH SET proc.exited_at = {at}, proc.exited_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid, at => At, at_mcs => Mcs} },

    % unlink all links with this process if existed
    { "MATCH (proc:Process)-[link:LINK]-(:Process)\n"
      "WHERE proc.instance_id = {id} AND proc.pid = {pid}\n"
      "SET link.unlinked_at = {at}, link.unlinked_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid, at => At, at_mcs => Mcs}}
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"link">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid, at => At, at_mcs => Mcs} },

    { "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid1, at => At, at_mcs => Mcs} },

    { "MATCH (proc1:Process { pid: {pid1}, instance_id: {id} }), (proc2:Process { pid: {pid2}, instance_id: {id} })\n"
      "CREATE (proc1)-[:LINK { linked_at: {at}, linked_at_mcs: {at_mcs} }]->(proc2)\n"
    , #{id => Id, pid1 => Pid, pid2 => Pid1, at => At, at_mcs => Mcs} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unlink">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid, at => At, at_mcs => Mcs} },
    { "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid1, at => At, at_mcs => Mcs} },

    { "MATCH (proc1:Process)-[link:LINK]-(proc2:Process)\n"
      "WHERE\tproc1.instance_id = {id} AND proc1.pid = {pid1} AND\n"
      "\tproc2.instance_id = {id} AND proc2.pid = {pid2} AND\n"
      "\tlink.unlinked_at = null\n"
      "SET link.unlinked_at = {at}, link.unlinked_at_mcs = {at_mcs}\n"
    , #{id => Id, pid1 => Pid, pid2 => Pid1, at => At, at_mcs => Mcs} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"register">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"atom">> := Atom} = E,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegAtom { atom: {atom}, instance_id: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
      "CREATE (reg)-[:REGISTER { registered_at: {at}, registered_at_mcs: {at_mcs} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At, at_mcs => Mcs} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unregister">>} = E | Events], Acc) ->
  #{<<"at">> := At, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"atom">> := Atom} = E,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegAtom { atom: {atom}, instance_id: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instance_id: {id} })\n"
      "ON CREATE SET proc.first_mentioned_at = {at}, proc.first_mentioned_at_mcs = {at_mcs}\n"
      "MERGE (reg)-[rel:REGISTER]->(proc)\n"
      "SET rel.unregistered_at = {at}, rel.unregistered_at_mcs = {at_mcs}\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At, at_mcs => Mcs} }
  ],
  process_events(Id, Events, [Statements] ++ Acc).
