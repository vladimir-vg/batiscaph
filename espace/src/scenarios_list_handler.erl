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
  case get_items_list(1000) of
    {error, Reason} ->
      Body = iolist_to_binary(io_lib:format("~p", [Reason])),
      {ok, Req1} = cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], Body, Req),
      {ok, Req1, State};

    {ok, Items} ->
      Body = jsx:encode(Items),
      {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req),
      {ok, Req1, State}
  end.



% get sorted list of scenarios
% scenarios which dir doesn't look like a date go first (like 'learn-you-some-erlang')
get_items_list(MaxItems) ->
  DBName = espace:get_prop(clickhouse_dbname),
  SQL = iolist_to_binary([
    "SELECT instance_id, MIN(toUInt64(at)) AS min_at\n",
    "FROM `", DBName, "`.events\n",
    "GROUP BY instance_id\n",
    "ORDER BY min_at DESC\n",
    "LIMIT ", integer_to_binary(MaxItems), "\n",
    "FORMAT TabSeparatedWithNamesAndTypes\n"
  ]),
  {ok, Body} = clickhouse:execute(SQL),
  {ok, Items} = clickhouse:parse_rows(maps, Body),
  {ok, Items}.
