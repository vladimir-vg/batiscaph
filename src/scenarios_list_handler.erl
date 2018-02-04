-module(scenarios_list_handler).
-include_lib("kernel/include/file.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  case get_items_list(100) of
    % {error, Reason} ->
    %   Body = iolist_to_binary(io_lib:format("~p", [Reason])),
    %   {ok, Req1} = cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], Body, Req),
    %   {ok, Req1, State};

    {ok, Items} ->
      Body = jsx:encode(Items),
      {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req),
      {ok, Req1, State}
  end.



% get sorted list of scenarios
% scenarios which dir doesn't look like a date go first (like 'learn-you-some-erlang')
get_items_list(MaxItems) ->
  DBName = batiscaph:get_prop(clickhouse_dbname),
  SQL1 = iolist_to_binary([
    "SELECT instance_id\n",
    "FROM `", DBName, "`.events\n",
    "GROUP BY instance_id\n",
    "ORDER BY min(toUInt64(at_s)) DESC\n",
    "LIMIT ", integer_to_binary(MaxItems), "\n",
    "FORMAT TabSeparatedWithNamesAndTypes\n"
  ]),
  {ok, Body1} = clickhouse:execute(SQL1),
  case clickhouse:parse_rows(one_column_list, Body1) of
    {ok, []} -> {ok, []};
    {ok, Ids} ->
      SQL2 = iolist_to_binary([
      "SELECT instance_id, context\n"
      "FROM `", DBName, "`.events\n"
      "WHERE ", clickhouse:id_in_values_sql(<<"instance_id">>, Ids), "\n"
      "\tAND context != ''\n"
      "\tAND position(context, ' init_per_suite') == 0\n"
      "\tAND position(context, ' end_per_suite') == 0\n"
      "\tAND position(context, ' init_per_group') == 0\n"
      "\tAND position(context, ' end_per_group') == 0\n"
      "\tAND position(context, ' init_per_testcase') == 0\n"
      "\tAND position(context, ' end_per_testcase') == 0\n"
      "GROUP BY instance_id, context\n"
      "ORDER BY min(at_s) DESC\n"
      "FORMAT TabSeparatedWithNamesAndTypes\n"
      ]),

      {ok, Body2} = clickhouse:execute(SQL2),
      {ok, Contexts} = clickhouse:parse_rows({lists_prop, <<"instance_id">>}, Body2),
      Scenarios = turn_context_lines_into_trees(Ids, maps:from_list(Contexts)),

      {ok, Scenarios}
  end.




turn_context_lines_into_trees(Ids, Contexts) ->
  lists:map(fun (InstanceId) ->
    case maps:get(InstanceId, Contexts, undefined) of
      undefined -> [InstanceId, #{}];
      ContextsList -> [InstanceId, list_to_tree(ContextsList, #{})]
    end
  end, Ids).

list_to_tree([], Acc) -> Acc;
list_to_tree([Context | Rest], Acc) ->
  Parts = binary:split(Context, <<" ">>, [global]),
  Acc1 = parts_into_map(Parts, Acc),
  list_to_tree(Rest, Acc1).

parts_into_map([Part], Acc) ->
  Acc#{Part => true};
parts_into_map([Part | Rest], Acc) ->
  Map = maps:get(Part, Acc, #{}),
  Map1 = parts_into_map(Rest, Map),
  Acc#{Part => Map1}.
