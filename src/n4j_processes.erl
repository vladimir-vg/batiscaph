-module(n4j_processes).
-export([delta_json/1]).



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
%  * MENTION { at, context } -- context is just a string like: 'ancestors', 'monitor', 'shell'
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
    { "MATCH (p1:Process {instanceId: {id}})-[rel]-(:Process {instanceId: {id}})\n"
      "WHERE TYPE(rel) IN [\"MENTION\", \"TRACE_STARTED\", \"TRACE_STOPPED\"]\n"
      "WITH p1, rel\n"
      "ORDER BY rel.at\n"
      "WITH p1, COLLECT({at: rel.at, type: TYPE(rel)}) AS events\n"
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
