-module(es_events).
-export([create_table/0, drop_table/0, store/2, select/1]).



%%% This module handles work with clickhouse events table



columns() ->
  [
    {<<"at">>, <<"DateTime">>},
    {<<"at_mcs">>, <<"UInt32">>},
    {<<"instance_id">>, <<"String">>},
    {<<"pid">>, <<"String">>},
    {<<"type">>, <<"String">>},
    {<<"pid1">>, <<"String">>}
  ].

create_table() ->
  DBName = espace:get_prop(clickhouse_dbname),
  Columns = iolist_to_binary([["\t", Name, " ", Type, ",\n"] || {Name, Type} <- columns()]),
  SQL = iolist_to_binary([
    "CREATE TABLE `", DBName, "`.events (\n",
    Columns,
    "\tevent_date Date DEFAULT toDate(at)\n",
    ") ENGINE=MergeTree(event_date, (instance_id, type, pid, at), 8192)\n"
  ]),
  ok = clickhouse:execute(SQL),
  ok.

drop_table() ->
  DBName = espace:get_prop(clickhouse_dbname),
  SQL = iolist_to_binary(["DROP TABLE `", DBName, "`.events\n"]),
  ok = clickhouse:execute(SQL),
  ok.



store(InstanceId, Events) ->
  DBName = espace:get_prop(clickhouse_dbname),
  ok = clickhouse:insert(DBName, <<"events">>, columns(), Events, fun (Item) ->
    Item#{<<"instance_id">> => InstanceId}
  end),
  ok.



select(Opts) ->
  DBName = espace:get_prop(clickhouse_dbname),

  WhereClause = case where_cond(Opts) of
    none -> <<>>;
    Cond -> ["WHERE ", Cond, "\n"]
  end,

  LimitClause = case maps:get(limit, Opts, undefined) of
    undefined -> <<>>;
    N when is_integer(N) -> <<"LIMIT ", (integer_to_binary(N))/binary, "\n">>
  end,

  SQL = iolist_to_binary([
    "SELECT ", select_columns_sql(), "\n",
    "FROM `", DBName, "`.events\n",
    WhereClause,
    "ORDER BY at, at_mcs\n",
    LimitClause,
    "FORMAT TabSeparatedWithNamesAndTypes\n"
  ]),

  {ok, Body} = clickhouse:execute(SQL),
  {ok, Events} = clickhouse:parse_rows(maps, Body),
  {ok, Events}.



select_columns_sql() ->
  <<", ", Columns/binary>> = iolist_to_binary(select_columns_sql(columns())),
  Columns.

select_columns_sql([]) -> [];
select_columns_sql([{Name, <<"DateTime">>} | Rest]) ->
  [[", toUInt64(", Name, ") AS ", Name] | select_columns_sql(Rest)];
select_columns_sql([{Name, _Type} | Rest]) ->
  [[", ", Name] | select_columns_sql(Rest)].



where_cond(Opts) ->
  Conds = maps:fold(fun
    (limit, _Val, Acc) -> Acc; % ignore
    (instance_id, Val, Acc) ->
      Cond = [<<"(instance_id = '">>, Val, <<"')">>],
      [Cond | Acc];

    ('after', {At, Mcs}, Acc) ->
      Cond = [<<"((toUInt64(at), at_mcs) > (">>, integer_to_binary(At), <<", ">>, integer_to_binary(Mcs), <<"))">>],
      [Cond | Acc];

    (type_in, Vals, Acc) when is_list(Vals) ->
      Cond = ["(", clickhouse:id_in_values_sql(<<"type">>, Vals), ")"],
      [Cond | Acc];

    (Key, Val, _Acc) -> error({unknown_select_opt, Key, Val})
  end, [], Opts),

  case Conds of
    [] -> none;
    _ -> lists:join(<<" AND ">>, Conds)
  end.

