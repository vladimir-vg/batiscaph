-module(n4j_processes).
-export([delta_json/1, update/2, desired_event_types/0]).



%%%
%%% This module handles work with Neo4j Process nodes
%%% and their relationships
%%%



%
% Process node have following properties:
%  * instanceId -- mandatory
%  * pid -- mandatory
%  * appearedAt -- equals to spawnedAt or timestamp of first mention. Mandatory
%  * spawnedAt -- May be missing, present only if such event was actually collected.
%  * exitedAt -- same
%  * exitReason -- same
%
% Process may have following relationships to other processes:
%  * SPAWN { at }
%  * LINK { at }
%  * UNLINK { at }
%  * REGISTER { at } -- connected to a RegisteredName node
%  * UNREGISTER { at } -- same
%  * EXPLICIT_MENTION { at, context } -- context is just a string like: 'ancestors', 'monitor', 'shell'
%    if you link previously unknown process, you mention it implicitly and it will appear on the map untraced.
%    In some cases it might be useful to mention explicitly, when no special event occured (e.g. after process_info call)
%
% also Process may have relationships to himself:
%  * TRACE_STARTED { at }
%  * TRACE_STOPPED { at }
%



% This function selects info about processes according to given options
% and returns it in delta format that can be applied on frontend.
% Currently it can select only all processes.
delta_json(#{instance_id := Id}) ->
  Statements = [

    % for some weird reason OPTIONAL MATCH (p1)-[rel]-(p1)
    % returned null relationship that was impossible to filter out by WHERE
    % but got rid of this null value by using COLLECT, and then map it by EXTRACT

    { "MATCH (p1:Process {instanceId: {id}})\n"
      "OPTIONAL MATCH (p1:Process)-[rel]-(p1:Process)\n"
      "WHERE TYPE(rel) IN [\"TRACE_STARTED\", \"TRACE_STOPPED\"]\n"
      "WITH p1, rel\n"
      "ORDER BY rel.at\n"
      "WITH p1, EXTRACT(r in COLLECT(rel) | {at: r.at, type: TYPE(r)}) AS events\n"
      "ORDER BY p1.appearedAt\n"
      "RETURN p1.appearedAt AS appearedAt, p1.pid AS pid, p1.spawnedAt AS spawnedAt, p1.exitedAt AS exitedAt, p1.exitReason AS exitReason, events\n"
    , #{id => Id} },

    { "MATCH (p1:Process {instanceId: {id}})-[rel]->(p2:Process {instanceId: {id}})\n"
      "RETURN rel.at AS at, p1.pid AS pid1, p2.pid AS pid2, TYPE(rel) AS type\n"
      "ORDER BY rel.at\n"
    , #{id => Id} }
  ],
  {ok, [Processes, Events]} = neo4j:commit(Statements),
  Processes1 = convert_rows_to_objects(Processes),
  Events1 = convert_rows_to_objects(Events),
  #{processes => Processes1, events => Events1}.



convert_rows_to_objects(#{<<"columns">> := Cols, <<"data">> := Rows}) ->
  lists:map(fun (#{<<"row">> := Vals}) ->
    maps:from_list(lists:zip(Cols, Vals))
  end, Rows).



update(Id, Events) ->
  Statements = process_events(Id, Events, []),
  case Statements of
    [] -> ok;
    _ -> {ok, _} = neo4j:commit(Statements), ok
  end.



desired_event_types() ->
  [<<"spawn">>, <<"exit">>, <<"link">>, <<"unlink">>, <<"register">>, <<"unregister">>, <<"trace_started">>, <<"trace_stopped">>].



process_events(_Id, [], Acc) -> lists:flatten(lists:reverse(Acc));

process_events(Id, [#{<<"type">> := <<"spawn">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid1">> := Parent, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create parent process if not existed before
    { "MERGE (parent:Process { pid: {parent}, instanceId: {id} })\n"
      "ON CREATE SET parent.appearedAt = {at}\n"
    , #{id => Id, parent => Parent, at => At} },

    % create new process
    { "MATCH (parent:Process)\n"
      "WHERE parent.instanceId = {id} AND parent.pid = {parent}\n"
      "CREATE\t(proc:Process { instanceId: {id}, pid: {pid}, spawnedAt: {at}, appearedAt: {at} }),\n"
      "\t(parent)-[:SPAWN { at: {at} }]->(proc)\n"
    , #{id => Id, parent => Parent, at => At, pid => Pid} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"exit">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"term">> := Reason} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.exitedAt = {at}, proc.exitReason = {reason}, proc.appearedAt = {at}\n"
      "ON MATCH SET proc.exitedAt = {at}, proc.exitReason = {reason}\n"
    , #{id => Id, pid => Pid, at => At, reason => Reason} }

    % % unlink all links with this process if existed
    % { "MATCH (proc:Process)-[link:LINK]-(:Process)\n"
    %   "WHERE proc.instanceId = {id} AND proc.pid = {pid}\n"
    %   "SET link.unlinked_at = {at}, link.unlinked_at_mcs = {at_mcs}\n"
    % , #{id => Id, pid => Pid, at => AtS, at_mcs => Mcs}}
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"link">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
    , #{id => Id, pid => Pid, at => At} },

    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
    , #{id => Id, pid => Pid1, at => At} },

    { "MATCH (proc1:Process { pid: {pid1}, instanceId: {id} }), (proc2:Process { pid: {pid2}, instanceId: {id} })\n"
      "CREATE (proc1)-[:LINK { at: {at} }]->(proc2)\n"
    , #{id => Id, pid1 => Pid, pid2 => Pid1, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unlink">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
    , #{id => Id, pid => Pid, at => At} },
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
    , #{id => Id, pid => Pid1, at => At} },

    { "MATCH (proc1:Process { pid: {pid1}, instanceId: {id} }), (proc2:Process { pid: {pid2}, instanceId: {id} })\n"
      "CREATE (proc1)-[:UNLINK { at: {at} }]->(proc2)\n"
    , #{id => Id, pid1 => Pid, pid2 => Pid1, at => At} }

    % { "MATCH (proc1:Process)-[link:LINK]-(proc2:Process)\n"
    %   "WHERE\tproc1.instanceId = {id} AND proc1.pid = {pid1} AND\n"
    %   "\tproc2.instanceId = {id} AND proc2.pid = {pid2} AND\n"
    %   "\tlink.unlinked_at = null\n"
    %   "SET link.unlinked_at = {at}, link.unlinked_at_mcs = {at_mcs}\n"
    % , #{id => Id, pid1 => Pid, pid2 => Pid1, at => AtS, at_mcs => Mcs} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"register">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"atom">> := Atom} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegisteredName { atom: {atom}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
      "CREATE (reg)-[:REGISTER { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unregister">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"atom">> := Atom} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegisteredName { atom: {atom}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
      "CREATE (reg)-[:UNREGISTER { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"trace_started">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
      "CREATE (proc)-[:TRACE_STARTED { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"trace_stopped">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}\n"
      "CREATE (proc)-[:TRACE_STOPPED { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc).
