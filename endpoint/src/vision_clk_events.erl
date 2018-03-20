-module(vision_clk_events).
-export([create_tables/0, drop_tables/0]).
-export([insert/1]).

% different select queries
-export([
  select_instances_infos_with_ids/1,
  select_events/1,
  select_plug_request_info/1
]).



columns() ->
  [
    {<<"AtSec">>, <<"DateTime">>},
    {<<"AtMcs">>, <<"UInt32">>},
    % id of event for given AtSec, AtMcs and InstanceId
    % need to distinguish several events that occured at exactly same time (AtSec,AtMcs)
    {<<"SubId">>, <<"UInt16">>},
    {<<"InstanceId">>, <<"String">>},
    {<<"Type">>, <<"String">>},
    {<<"Pid1">>, <<"String">>},
    {<<"Pid2">>, <<"String">>},

    % we gonna insert nested columns just as two arrays
    {<<"Attrs.Key">>, {array, <<"String">>}},
    {<<"Attrs.Value">>, {array, <<"String">>}}
  ].



create_tables() ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  SQL = iolist_to_binary([
    "CREATE TABLE `", DBName, "`.events (\n",
    "\tAtSec DateTime,\n",
    "\tAtMcs UInt32,\n",
    "\tSubId UInt16,\n",
    % might be better to turn it into FixedString(32)
    "\tInstanceId String,\n",
    "\tType String,\n",
    "\tPid1 String,\n",
    "\tPid2 String,\n",

    % into this nested column fall all custom fields
    "\tAttrs Nested (\n",
    "\t\tKey String,\n",
    "\t\tValue String\n",
    "\t),\n",
    "\tEventDate Date DEFAULT toDate(AtSec)\n",
    ") ENGINE=MergeTree(EventDate, (InstanceId, (AtSec, AtMcs)), 8192)\n"
  ]),
  ok = clickhouse:execute(SQL),
  ok.



drop_tables() ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  SQL = iolist_to_binary([
    "DROP TABLE `", DBName, "`.events\n"
  ]),
  clickhouse:execute(SQL),
  ok.



insert(Events) ->
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  InsertedColumns = [
    <<"InstanceId">>, <<"AtSec">>, <<"AtMcs">>, <<"SubId">>, <<"Type">>,
    <<"Pid1">>, <<"Attrs.Key">>, <<"Attrs.Value">>
  ],
  Columns = [C || {K, _} = C <- columns(), lists:member(K, InsertedColumns)],
  ok = clickhouse:insert(DBName, <<"events">>, Columns, Events),
  ok.



select_instances_infos_with_ids([]) -> {ok, []};
select_instances_infos_with_ids(Ids) ->
  {ok, Q} = application:get_env(vision, clk_queries),
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  {ok, SQL} = eql:get_query(select_instances_infos_with_ids, Q, [{dbname, DBName}, {ids, clickhouse:list_sql(Ids)}]),
  {ok, Body} = clickhouse:execute(SQL),
  {ok, Events} = clickhouse:parse_rows(maps, Body),

  CurrentlyConnected = [Id || {Id, _Attrs} <- gen_tracker:list(probes, [])],

  Instances = lists:foldl(fun
    (#{<<"Type">> := <<"0 vision connection-start">>, <<"InstanceId">> := Id, <<"At">> := At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Map1 = Map#{<<"Connected">> => lists:member(Id, CurrentlyConnected)},
      Acc#{Id => maps:put(<<"StartedAt">>, At, Map1)};

    (#{<<"Type">> := <<"0 vision connection-stop">>, <<"InstanceId">> := Id, <<"At">> := At}, Acc) ->
      Map = maps:get(Id, Acc, #{<<"InstanceId">> => Id}),
      Acc#{Id => maps:put(<<"StoppedAt">>, At, Map)}
  end, #{}, Events),

  {ok, maps:values(Instances)}.



select_events(#{
  instance_id := Id, types := Types, attrs := Attrs,
  earlier_than := now, limit := Limit
}) ->
  {ok, Q} = application:get_env(vision, clk_queries),
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),
  Params = [
    {dbname, DBName}, {types, clickhouse:list_sql(Types)},
    {instance_id, Id}, {limit, integer_to_binary(Limit)},
    {attrs, attrs_sql(Q, Attrs)}
  ],
  {ok, SQL} = eql:get_query(select_events_from_now, Q, Params),

  {ok, Body} = clickhouse:execute(SQL),
  {ok, Events} = clickhouse:parse_rows(maps, Body),

  {ok, Events}.

attrs_sql(Q, Attrs) ->
  Parts = lists:map(fun (Attr) ->
    {ok, SQL} = eql:get_query(selectable_attr, Q, [{attr, Attr}]),
    SQL
  end, Attrs),
  lists:join(<<",">>, Parts).



select_plug_request_info(#{
  pid := Pid, instance_id := InstanceId,
  started_at := StartedAt, stopped_at := StoppedAt
}) ->
  {ok, Q} = application:get_env(vision, clk_queries),
  {ok, DBName} = application:get_env(vision, clickhouse_dbname),

  Attrs = [<<"req_headers">>, <<"resp_headers">>, <<"resp_code">>, <<"method">>, <<"port">>, <<"path">>, <<"module">>],
  Types = [
    <<"p1 plug:request start">>, <<"p1 plug:request stop">>,
    <<"p1 plug:plug start">>, <<"p1 plug:plug stop">>
  ],
  Params = [
    {dbname, DBName}, {instance_id, InstanceId}, {pid, Pid},
    {started_at, integer_to_binary(StartedAt)}, {stopped_at, integer_to_binary(StoppedAt)},
    {types, clickhouse:list_sql(Types)}, {attrs, attrs_sql(Q, Attrs)}
  ],

  {ok, SQL} = eql:get_query(select_request_events, Q, Params),
  {ok, Body} = clickhouse:execute(SQL),
  {ok, Events} = clickhouse:parse_rows(maps, Body),
  Info = produce_request_info(Events, #{<<"Pid">> => Pid}),
  {ok, Info}.



produce_request_info([], R) ->
  case maps:get(plugs, R, undefined) of
    undefined ->
      maps:without([current_plugs_stack], R);

    [_ | _] = Plugs ->
      maps:without([current_plugs_stack], R#{plugs => lists:reverse(Plugs)})
  end;

produce_request_info([#{<<"Type">> := <<"p1 plug:request start">>} = E | Events], R) ->
  #{<<"At">> := At, <<"method">> := Method,
    <<"path">> := Path, <<"req_headers">> := Headers
  } = E,
  R1 = R#{
    <<"StartedAt">> => At, <<"method">> => Method,
    <<"path">> => Path, <<"req_headers">> => read_headers(Headers)
  },
  produce_request_info(Events, R1);

produce_request_info([#{<<"Type">> := <<"p1 plug:plug start">>} = E | Events], R) ->
  #{<<"At">> := At, <<"module">> := Module} = E,
  PlugsStack = maps:get(current_plugs_stack, R, []),
  P = #{<<"StartedAt">> => At, <<"module">> => Module},
  PlugsStack1 = [P | PlugsStack],
  R1 = R#{current_plugs_stack => PlugsStack1},
  produce_request_info(Events, R1);

produce_request_info([#{<<"Type">> := <<"p1 plug:plug stop">>} = E | Events], R) ->
  #{<<"At">> := At, <<"module">> := Module} = E,
  [#{<<"module">> := Module} = P | PlugsStack] = maps:get(current_plugs_stack, R),
  P1 = P#{<<"StoppedAt">> => At},
  P2 =
    case maps:get(plugs, P1, undefined) of
      undefined -> P1;
      NestedPlugs -> P1#{plugs => lists:reverse(NestedPlugs)}
    end,

  case PlugsStack of
    [] ->
      ReqPlugs = maps:get(plugs, R, []),
      R1 = R#{plugs => [P2 | ReqPlugs], current_plugs_stack => []},
      produce_request_info(Events, R1);

    [ParentPlug | Rest] ->
      SiblingsPlugs = maps:get(plugs, ParentPlug, []),
      ParentPlug1 = ParentPlug#{plugs => [P2 | SiblingsPlugs]},
      R1 = R#{current_plugs_stack => [ParentPlug1 | Rest]},
      produce_request_info(Events, R1)
  end;

produce_request_info([#{<<"Type">> := <<"p1 plug:request stop">>} = E | Events], R) ->
  #{<<"At">> := StoppedAt,
    <<"resp_code">> := Code, <<"resp_headers">> := Headers
  } = E,
  R1 = R#{
    <<"StoppedAt">> => StoppedAt, <<"resp_code">> => Code,
    <<"resp_headers">> => read_headers(Headers)
  },
  produce_request_info(Events, R1).

read_headers(Headers) ->
  try erlang:binary_to_term(Headers, [safe]) of
    Value -> [[K, V] || {K, V} <- Value]
  catch _:_ -> <<"unable_to_decode_binary_term">>
  end.



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