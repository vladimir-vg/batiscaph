-module(clickhouse).
-export([execute/1, insert/4, insert/5]).
-export([id_in_values_sql/2, parse_rows/2]).
-define(TIMEOUT, 10000).



insert(DBName, Table, Columns, Events) ->
  insert(DBName, Table, Columns, Events, undefined).

insert(DBName, Table, Columns, Events, ItemPrepareFun) ->
  <<",", ColNames/binary>> = iolist_to_binary([[",", Name] || {Name,_Type} <- Columns]),
  SQL = iolist_to_binary(["INSERT INTO `", DBName, "`.`", Table, "` (", ColNames, ") FORMAT TabSeparated\n"]),
  {ok, Rows} = items_to_tsv(Columns, Events, ItemPrepareFun),
  ok = execute(SQL, Rows),
  ok.


execute(SQL) ->
  execute(SQL, <<>>).

execute(SQL, Suffix) ->
  URL = batiscaph:get_prop(clickhouse_url),
  Body = <<SQL/binary, Suffix/binary>>,
  % suffix is usually terribly long (inserted rows, etc) and shouldn't be visible in logs
  lager:info("Clickhouse request:\n~s", [SQL]),
  case hackney:post(URL, [], Body, [{timeout, ?TIMEOUT}]) of
    {ok, 500, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> {error, unknown_500};
        {ok, RespBody} when is_binary(Body) ->
          lager:error("Clickhouse error:\n~s", [RespBody]),
          {error, RespBody}
      end;

    {ok, 200, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> ok;
        {ok, RespBody} when is_binary(RespBody) -> {ok, RespBody}
      end
  end.



items_to_tsv(Columns, Items, ItemPrepareFun) ->
  items_to_tsv(Columns, Items, ItemPrepareFun, <<>>).

items_to_tsv(_Columns, [], _, Acc) ->
  {ok, Acc};

items_to_tsv(Columns, [Item | Items], undefined, Acc) ->
  Row = item_to_row(Columns, Item),
  items_to_tsv(Columns, Items, undefined, <<Acc/binary, Row/binary, "\n">>);

items_to_tsv(Columns, [Item | Items], ItemPrepareFun, Acc) when is_function(ItemPrepareFun) ->
  Row = item_to_row(Columns, ItemPrepareFun(Item)),
  items_to_tsv(Columns, Items, ItemPrepareFun, <<Acc/binary, Row/binary, "\n">>).

item_to_row(Columns, Item) ->
  <<"\t", Row/binary>> = iolist_to_binary([["\t", stringify_value(Name, Type, Item)] || {Name, Type} <- Columns]),
  Row.



stringify_value(Name, Type, Item) ->
  DefaultValue = default_value(Type),
  Value = maps:get(Name, Item, DefaultValue),
  stringify_value0(Value, Type).

default_value(<<"String">>) -> <<>>;
default_value(<<"DateTime">>) -> <<"0000000000">>;
default_value(<<"UInt8">>) -> 0;
default_value(<<"UInt16">>) -> 0;
default_value(<<"UInt32">>) -> 0;
default_value(<<"UInt64">>) -> 0;
default_value(<<"Int8">>) -> 0;
default_value(<<"Int16">>) -> 0;
default_value(<<"Int32">>) -> 0;
default_value(<<"Int64">>) -> 0;
default_value(Type) -> error({unknown_default_column_value, Type}).

stringify_value0(Value, <<"String">>) -> escape_string(Value);
% Clickhouse accepts unixtime as datetime
stringify_value0(Value, <<"DateTime">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"UInt8">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"UInt16">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"UInt32">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"UInt64">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"Int8">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"Int16">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"Int32">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, <<"Int64">>) when is_integer(Value) -> integer_to_binary(Value);
stringify_value0(Value, Type) -> error({stringify_not_implemented, Value, Type}).



escape_string(Value) when is_list(Value) -> escape_string(list_to_binary(Value));
escape_string(Value) -> escape_string(Value, <<>>).

escape_string(<<>>, Acc) -> Acc;
escape_string(<<"\n", Rest/binary>>, Acc) ->
  escape_string(Rest, <<Acc/binary, "\\\n">>);
escape_string(<<"\t", Rest/binary>>, Acc) ->
  escape_string(Rest, <<Acc/binary, "\\\t">>);
escape_string(<<"\\", Rest/binary>>, Acc) ->
  escape_string(Rest, <<Acc/binary, "\\\\">>);
% visible ASCII text range
escape_string(<<A, Rest/binary>>, Acc) when A >= 16#20, A =< 16#7e ->
  escape_string(Rest, <<Acc/binary, A>>);
escape_string(<<A, Rest/binary>>, Acc) when A < 16 ->
  escape_string(Rest, <<Acc/binary, "\\x0", (integer_to_binary(A, 16))/binary>>);
escape_string(<<A, Rest/binary>>, Acc) ->
  escape_string(Rest, <<Acc/binary, "\\x", (integer_to_binary(A, 16))/binary>>).



id_in_values_sql(Key, Values) ->
  <<",", Values1/binary>> = iolist_to_binary([[",'", escape_string(V), "'"] || V <- Values]),
  [Key, " IN (", Values1, ")"].



% Binary supposed to be in TabSeparatedWithNamesAndTypes format
parse_rows(ReturnType, Binary) ->
  [ColumnsRaw, Rest1] = binary:split(Binary, <<"\n">>),
  Columns = binary:split(ColumnsRaw, <<"\t">>, [global]),
  [TypesRaw, Rest2] = binary:split(Rest1, <<"\n">>),
  Types = binary:split(TypesRaw, <<"\t">>, [global]),
  parse_rows(ReturnType, lists:zip(Columns, Types), Rest2, []).

% currently no other return types supported than 'maps'
parse_rows(maps, _Columns, <<>>, Acc) -> {ok, lists:reverse(Acc)};
parse_rows(ReturnType, Columns, Binary, Acc) ->
  {Row, Rest} = read_row(Columns, Binary),
  Acc1 = collect_row(ReturnType, Columns, Row, Acc),
  parse_rows(ReturnType, Columns, Rest, Acc1).

read_row(Columns, Binary) ->
  {Row, Rest} = lists:foldl(fun ({_Name, Type}, {Row, Rest1}) ->
    {Value, Rest2} = read_value(Type, Rest1),
    {[Value | Row], Rest2}
  end, {[], Binary}, Columns),
  {lists:reverse(Row), Rest}.



read_value(<<"DateTime">>, Binary) -> read_int(Binary);
read_value(<<"UInt64">>, Binary) -> read_int(Binary);
read_value(<<"UInt32">>, Binary) -> read_int(Binary);
read_value(<<"UInt16">>, Binary) -> read_int(Binary);
read_value(<<"UInt8">>, Binary) -> read_int(Binary);
read_value(<<"Int64">>, Binary) -> read_int(Binary);
read_value(<<"Int32">>, Binary) -> read_int(Binary);
read_value(<<"Int16">>, Binary) -> read_int(Binary);
read_value(<<"Int8">>, Binary) -> read_int(Binary);
read_value(<<"Float64">>, Binary) -> read_float(Binary);

read_value(<<"String">>, Binary) ->
  {ok, Val, Rest} = read_sting(Binary, <<>>),
  {Val, Rest}.



read_sting(<<>>, Acc) -> {ok, Acc, <<>>};
read_sting(<<"\\\t", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\t">>);
read_sting(<<"\\\n", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\n">>);
read_sting(<<"\\\r", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\r">>);
read_sting(<<"\\\\", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\\">>);
read_sting(<<"\\t", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\t">>);
read_sting(<<"\\n", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\n">>);
read_sting(<<"\\r", Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, "\r">>);
read_sting(<<"\t", Rest/binary>>, Acc) -> {ok, Acc, Rest};
read_sting(<<"\n", Rest/binary>>, Acc) -> {ok, Acc, Rest};
read_sting(<<C, Rest/binary>>, Acc) -> read_sting(Rest, <<Acc/binary, C>>).



read_int(Binary) ->
  [Val, Rest] = binary:split(Binary, [<<"\t">>, <<"\n">>]),
  {binary_to_integer(Val), Rest}.



read_float(Binary) ->
  [Val, Rest] = binary:split(Binary, [<<"\t">>, <<"\n">>]),
  Val1 =
    try binary_to_float(Val)
    catch error:badarg -> binary_to_integer(Val)
    end,
  {Val1, Rest}.



collect_row(maps, Columns, Values, Acc) ->
  Row = lists:foldl(fun ({{Col, _Type}, Value}, Row1) ->
    Row1#{Col => Value}
  end, #{}, lists:zip(Columns, Values)),
  [Row | Acc].