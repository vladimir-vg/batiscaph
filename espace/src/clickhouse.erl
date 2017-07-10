-module(clickhouse).
-export([execute/1]).
-define(TIMEOUT, 10000).



execute(SQL) ->
  URL = espace:get_prop(clickhouse_url),
  lager:info("Clickhouse request:\n~s", [SQL]),
  case hackney:post(URL, [], SQL, [{timeout, ?TIMEOUT}]) of
    {ok, 500, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> {error, unknown_500};
        {ok, Body} when is_binary(Body) ->
          lager:error("Clickhouse error:\n~s", [Body]),
          {error, Body}
      end;

    {ok, 200, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> ok;
        {ok, Body} when is_binary(Body) -> {ok, Body}
      end
  end.