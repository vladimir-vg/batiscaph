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
%  * disappearedAt -- equals to exitedAt or timestamp of discovering that process is dead
%  * application -- application atom to which process belongs to. Might be empty.
%  * registeredName -- last known registered name for this process
%
% Process may have following relationships to other processes:
%  * SPAWN { at }
%  * LINK { at }
%  * UNLINK { at }
%  * REGISTER { at } -- connected to a RegisteredName node
%  * UNREGISTER { at } -- same
%  * FOUND_REGISTERED { at } -- same as REGISTER, but whe don't know when it happened, just discovered the fact
%  * MENTION { at, context } -- context is just a string like: 'ancestors', 'monitor', 'shell'
%    if you link previously unknown process, you mention it implicitly and it will appear on the map untraced.
%    In some cases it might be useful to mention explicitly, when no special event occured (e.g. after process_info call)
%
% also Process may have relationships to himself:
%  * TRACE_STARTED { at }
%  * TRACE_STOPPED { at }
%  * FOUND_DEAD { at } -- after failed test is_process_alive while trying to start trace
%
% Context node have following properties:
%  * instanceId
%  * startedAt
%  * stoppedAt
%  * context -- list of strings, for example: [suite, group1, group2, testcase]
%
% may have following relationships to Processes
%  * VAR_MENTION { at, expr } -- indicates that some process was mentioned in variable in context
%                            process might be somewhere nested in term, and have long expr like elment(4,Value)
%                            the root process for this context will be bounded as expr='self()'
%                            this relationship don't represent all variables values only process binds.



% This function selects info about processes according to given options
% and returns it in delta format that can be applied on frontend.
% Currently it can select only all processes.
delta_json(#{instance_id := Id, 'after' := At}) ->
  Statements = [
    % for some weird reason OPTIONAL MATCH (p1)-[rel]-(p1)
    % returned null relationship that was impossible to filter out by WHERE
    % but got rid of this null value by using COLLECT, and then map it by EXTRACT

    { "MATCH (p1:Process {instanceId: {id}})\n"
      "WHERE ((p1.disappearedAt IS NULL) OR p1.disappearedAt > {at})\n"
      "OPTIONAL MATCH (p1:Process)-[rel]-(p1:Process)\n"
      "WHERE TYPE(rel) IN ['TRACE_STARTED', 'TRACE_STOPPED', 'FOUND_DEAD'] AND (rel.at > {at})\n"
      % "OPTIONAL MATCH (p1:Process)-[bind:VAR_MENTION]-(context:Context)\n"
      % "WHERE bind.at > {at}\n"
      % "WITH p1, COLLECT({at: rel.at, type: TYPE(rel)}) + COLLECT({at: bind.at, type: TYPE(bind), expr: bind.expr, context: context.context}) AS events\n"
      % "UNWIND events AS event\n"
      "WITH p1, rel\n"
      "ORDER BY rel.at\n"
      "WITH p1, FILTER(e IN COLLECT({at: rel.at, type: TYPE(rel)}) WHERE e.at IS NOT NULL) AS events\n"
      "ORDER BY p1.appearedAt\n"
      "RETURN p1.appearedAt AS appearedAt, p1.pid AS pid, p1.spawnedAt AS spawnedAt, p1.exitedAt AS exitedAt, p1.exitReason AS exitReason, p1.disappearedAt AS disappearedAt, p1.application AS application, p1.registeredName AS registeredName, events\n"
    , #{id => Id, at => At} },

    { "MATCH (p1:Process {instanceId: {id}})-[rel]->(p2:Process {instanceId: {id}})\n"
      "WHERE NOT TYPE(rel) IN [\"TRACE_STARTED\", \"TRACE_STOPPED\", \"FOUND_DEAD\"] AND rel.at > {at}\n"
      "RETURN rel.at AS at, p1.pid AS pid1, p2.pid AS pid2, TYPE(rel) AS type\n"
      "ORDER BY rel.at\n"
    , #{id => Id, at => At} },

    { "MATCH (p:Process {instanceId: {id}})-[rel:VAR_MENTION]-(c:Context {instanceId: {id}})\n"
      "WHERE rel.at > {at}\n"
      "RETURN rel.at AS at, c.pid AS pid1, p.pid AS pid2, TYPE(rel) AS type, c.context AS context, rel.expr AS expr\n"
      "ORDER BY rel.at\n"
    , #{id => Id, at => At} },

    { "MATCH (context:Context { instanceId: {id} })\n"
      "WHERE ((context.stoppedAt IS NULL) OR context.stoppedAt > {at})\n"
      % "MATCH (context)-[bind:VAR_MENTION]->(proc:Process)\n"
      % "WITH COLLECT({at: bind.at, expr: bind.expr, pid: proc.pid}) AS binds, context\n"
      "RETURN context.startedAt AS startedAt, context.stoppedAt AS stoppedAt, context.context AS context, context.pid AS pid\n"
    , #{id => Id, at => At} }
  ],
  {ok, [Processes, Events, ContextMentionEvents, Contexts]} = neo4j:commit(Statements),

  Processes1 = convert_rows_to_objects(Processes),
  Events1 = convert_rows_to_objects(Events),
  ContextMentionEvents1 = convert_rows_to_objects(ContextMentionEvents),
  Contexts1 = convert_rows_to_map(<<"context">>, Contexts),

  Events2 = lists:sort(fun (#{<<"at">> := A}, #{<<"at">> := B}) ->
    A < B
  end, Events1 ++ ContextMentionEvents1),

  {ok, #{processes => Processes1, events => Events2, contexts => Contexts1}};

delta_json(Opts) ->
  delta_json(Opts#{'after' => 0}).



convert_rows_to_objects(#{<<"columns">> := Cols, <<"data">> := Rows}) ->
  lists:map(fun (#{<<"row">> := Vals}) ->
    maps:from_list(lists:zip(Cols, Vals))
  end, Rows).

convert_rows_to_map(ColumnKey, #{<<"columns">> := Cols, <<"data">> := Rows}) ->
  lists:foldl(fun (#{<<"row">> := Vals}, Acc) ->
    Obj = maps:from_list(lists:zip(Cols, Vals)),
    Key = maps:get(ColumnKey, Obj),
    Acc#{Key => Obj}
  end, #{}, Rows).



update(Id, Events) ->
  Statements = process_events(Id, Events, []),
  case Statements of
    [] -> ok;
    _ -> {ok, _} = neo4j:commit(Statements), ok
  end.



desired_event_types() ->
  [
    <<"spawn">>, <<"exit">>, <<"link">>, <<"unlink">>, <<"register">>, <<"unregister">>,
    <<"trace_started">>, <<"trace_stopped">>, <<"found_dead">>, <<"mention">>,
    <<"context_start">>, <<"context_stop">>, <<"var_mention">>
  ].



process_events(_Id, [], Acc) -> lists:flatten(lists:reverse(Acc));

process_events(Id, [#{<<"type">> := <<"spawn">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid1">> := Parent, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  ParentKey = <<Id/binary,"/",Parent/binary>>,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create parent process if not existed before
    { "MERGE (parent:Process { pid: {parent}, instanceId: {id} })\n"
      "ON CREATE SET parent.appearedAt = {at}, parent.key = {parent_key}\n"
    , #{id => Id, parent => Parent, at => At, parent_key => ParentKey} },

    % create new process
    { "MATCH (parent:Process {pid: {parent}, instanceId: {id}})\n"
      % "WHERE parent.instanceId = {id} AND parent.pid = {parent}\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.spawnedAt = {at}, proc.appearedAt = {at}, proc.key = {key}\n"
      "ON MATCH SET proc.spawnedAt = {at}\n"
      "CREATE (parent)-[:SPAWN { at: {at} }]->(proc)\n"
      % "CREATE\t(proc:Process { instanceId: {id}, pid: {pid}, spawnedAt: {at}, appearedAt: {at} }),\n"
      % "\t\n"
    , #{id => Id, parent => Parent, at => At, pid => Pid, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"exit">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"term">> := Reason} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.exitedAt = {at}, proc.exitReason = {reason}, proc.appearedAt = {at}, proc.disappearedAt = {at}, proc.key = {key}\n"
      "ON MATCH SET proc.exitedAt = {at}, proc.exitReason = {reason}, proc.disappearedAt = {at}\n"
    , #{id => Id, pid => Pid, at => At, reason => Reason, key => Key} }

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
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
    , #{id => Id, pid => Pid, at => At, key => Key} },

    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
    , #{id => Id, pid => Pid1, at => At, key => Key} },

    { "MATCH (proc1:Process { pid: {pid1}, instanceId: {id} }), (proc2:Process { pid: {pid2}, instanceId: {id} })\n"
      "CREATE (proc1)-[:LINK { at: {at} }]->(proc2)\n"
    , #{id => Id, pid1 => Pid, pid2 => Pid1, at => At} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unlink">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create both process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
    , #{id => Id, pid => Pid, at => At, key => Key} },
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
    , #{id => Id, pid => Pid1, at => At, key => Key} },

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
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegisteredName { atom: {atom}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}, proc.registeredName = {atom}\n"
      "ON MATCH SET proc.registeredName = {atom}\n"
      "CREATE (reg)-[:REGISTER { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"unregister">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"atom">> := Atom} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create process and atom node, connect them
    { "MERGE (reg:RegisteredName { atom: {atom}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      "CREATE (reg)-[:UNREGISTER { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => Atom, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"trace_started">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  App = case maps:get(<<"application">>, E, <<>>) of
    <<>> -> null;
    App1 when is_binary(App1) -> App1
  end,
  Statements = [
    % create process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}, proc.application = {application}\n"
      "CREATE (proc)-[:TRACE_STARTED { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, at => At, key => Key, application => App} }
  ],
  % Ancestors1 = binary:split(Ancestors, <<" ">>, [global]),
  Statements1 = Statements ++ registered_name_statements(Id, At, Pid, maps:get(<<"atom">>, E, <<>>)),
  process_events(Id, Events, [Statements1] ++ Acc);

process_events(Id, [#{<<"type">> := <<"trace_stopped">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    % create parent process if not existed before
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      "CREATE (proc)-[:TRACE_STOPPED { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"found_dead">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      "CREATE (proc)-[:FOUND_DEAD { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"mention">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"pid1">> := Pid1} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Key1 = <<Id/binary,"/",Pid1/binary>>,
  Statements = [
    { "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      "MERGE (proc1:Process { pid: {pid1}, instanceId: {id} })\n"
      "ON CREATE SET proc1.appearedAt = {at}, proc1.key = {key1}\n"
      "CREATE (proc)-[:MENTION { at: {at} }]->(proc1)\n"
    , #{id => Id, pid => Pid, pid1 => Pid1, at => At, key => Key, key1 => Key1} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"context_start">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"context">> := Context} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    { "MERGE (context:Context { context: {context}, instanceId: {id}, pid: {pid} })\n"
      "ON CREATE SET context.startedAt = {at}\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      % "CREATE (context)-[:VAR_MENTION { at: {at}, expr: 'self()' }]->(proc)\n"
    , #{id => Id, pid => Pid, context => Context, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"context_stop">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid">> := Pid, <<"context">> := Context} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    { "MERGE (context:Context { context: {context}, instanceId: {id}, pid: {pid} })\n"
      "ON MATCH SET context.stoppedAt = {at}\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
    , #{id => Id, pid => Pid, context => Context, at => At, key => Key} }
  ],
  process_events(Id, Events, [Statements] ++ Acc);

process_events(Id, [#{<<"type">> := <<"var_mention">>} = E | Events], Acc) ->
  #{<<"at_s">> := AtS, <<"at_mcs">> := Mcs, <<"pid1">> := Pid, <<"context">> := Context, <<"term">> := Expr} = E,
  At = AtS*1000*1000 + Mcs,
  Key = <<Id/binary,"/",Pid/binary>>,
  Statements = [
    { "MATCH (context:Context { context: {context}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}\n"
      "CREATE (context)-[:VAR_MENTION { at: {at}, expr: {expr} }]->(proc)\n"
    , #{id => Id, pid => Pid, context => Context, at => At, key => Key, expr => Expr} }
  ],
  process_events(Id, Events, [Statements] ++ Acc).



registered_name_statements(_Id, _At, _Pid, <<>>) -> [];
registered_name_statements(Id, At, Pid, RegName) when is_binary(RegName) ->
  Key = <<Id/binary,"/",Pid/binary>>,
  [
    % create process and atom node, connect them
    { "MERGE (reg:RegisteredName { atom: {atom}, instanceId: {id} })\n"
      "MERGE (proc:Process { pid: {pid}, instanceId: {id} })\n"
      "ON CREATE SET proc.appearedAt = {at}, proc.key = {key}, proc.registeredName = {atom}\n"
      "ON MATCH SET proc.registeredName = {atom}\n"
      "CREATE (reg)-[:FOUND_REGISTERED { at: {at} }]->(proc)\n"
    , #{id => Id, pid => Pid, atom => RegName, at => At, key => Key} }
  ].



% ancestors_mentions(Id, At, Pid, Ancestors) ->
%   lists:flatmap(fun
%     (<<"<", _/binary>> = MentionPid) ->
%       Key = <<Id/binary,"/",MentionPid/binary>>,
%       [
%         { "MATCH (proc:Process { pid: {pid1}, instanceId: {id} })\n"
%           "MERGE (mproc:Process { pid: {pid2}, instanceId: {id} })\n"
%           "ON CREATE SET mproc.appearedAt = {at}, mproc.key = {key}\n"
%           "CREATE (proc)-[:EXPLICIT_MENTION { at: {at}, context: 'ancestors' }]->(mproc)\n"
%         , #{id => Id, pid1 => Pid, pid2 => MentionPid, at => At, key => Key}}
%       ];
%     (_RegName) -> []
%   end, Ancestors).
