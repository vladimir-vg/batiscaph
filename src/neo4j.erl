-module(neo4j).
-export([create_index/2, commit/1]).
-define(TIMEOUT, 5000).



-spec create_index(binary(), [binary()]) -> ok.
create_index(Label, Props) ->
  case post_json(<<"db/data/schema/index/", Label/binary>>, #{<<"property_keys">> => Props}) of
    {error, Reason} -> {error, Reason};
    {ok, #{<<"label">> := Label, <<"property_keys">> := [_|_]}} -> ok
  end.



% transaction interface, executed in one request
-spec commit([{binary(), map()}]) -> ok.
commit(Statements) ->
  Statements1 = lists:map(fun
    ({Stmt, Params}) when is_binary(Stmt) ->
      #{statement => Stmt, parameters => Params};
    ({Stmt, Params}) when is_list(Stmt) ->
      #{statement => list_to_binary(Stmt), parameters => Params}
  end, Statements),

  StatementsText = [[Stmt, jsx:encode(Params), "\n\n"] || {Stmt, Params} <- Statements],

  lager:info("Neo4j commit:\n~s", [StatementsText]),
  case post_json(<<"db/data/transaction/commit">>, #{statements => Statements1}) of
    {ok, #{} = RespJSON} -> {ok, RespJSON}
  end.




post_json(Path, ReqJSON) ->
  BaseURL = espace:get_prop(neo4j_url),
  URL = <<BaseURL/binary, Path/binary>>,
  Headers = [{<<"Accept">>, <<"application/json; charset=UTF-8">>}, {<<"Content-Type">>, <<"application/json">>}],
  % lager:info("Neo4j POST ~p\n~s\n\n", [Path, jsx:encode(ReqJSON, [{indent,2}])]),
  case hackney:post(URL, Headers, jsx:encode(ReqJSON), [{timeout, ?TIMEOUT}]) of
    {ok, 409, _RespHeaders, ClientRef} ->
      {ok, RespBody} = hackney:body(ClientRef),
      JSON = jsx:decode(RespBody, [return_maps]),
      {error, {conflict, JSON}};

    {ok, 200, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> ok;
        {ok, RespBody} when is_binary(RespBody) ->
          RespJSON = jsx:decode(RespBody, [return_maps]),
          case maps:get(<<"errors">>, RespJSON, undefined) of
            undefined -> {ok, RespJSON};
            [] -> {ok, RespJSON};
            [_|_] ->
              lager:error("Neo4j POST error ~p:~n~p", [Path, RespJSON]),
              {error, RespJSON}
          end
      end
  end.
