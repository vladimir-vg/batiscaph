-module(neo4j).
-export([create_index/2, commit/1]).
-define(TIMEOUT, 5000).



% This code is currently unused.
%
% Some time ago this project used Neo4J for building and storing graph
% of processes and their relationships. It was dropped as too early and complicated decision.
% Turns out that it's easier just to build maps and store them in memory, specific for client requests.



-spec create_index(binary(), [binary()]) -> ok.
create_index(Label, Props) ->
  case post_json(<<"db/data/schema/index/", Label/binary>>, #{<<"property_keys">> => Props}) of
    {error, Reason} -> {error, Reason};
    {ok, #{<<"label">> := Label, <<"property_keys">> := [_|_]}} -> ok
  end.



% transaction interface, executed in one request
-spec commit([{binary() | list(), map()}]) -> {ok, any()} | {error, any()}.
commit(Statements) ->
  Statements1 = lists:map(fun
    ({Stmt, Params}) when is_binary(Stmt) ->
      #{statement => Stmt, parameters => Params};
    ({Stmt, Params}) when is_list(Stmt) ->
      #{statement => list_to_binary(Stmt), parameters => Params}
  end, Statements),

  StatementsText = [[Stmt, jsx:encode(Params), "\n\n"] || {Stmt, Params} <- Statements],

  {TimeSpent, Result} = timer:tc(fun() ->
    post_json(<<"db/data/transaction/commit">>, #{statements => Statements1})
  end),
  lager:info("Neo4j commit (~pms):\n~s", [TimeSpent div 1000, StatementsText]),
  case Result of
    {ok, #{<<"errors">> := [], <<"results">> := Results}} -> {ok, Results};
    {ok, #{<<"errors">> := Errors}} -> {error, Errors}
  end.




post_json(Path, ReqJSON) ->
  BaseURL = batiscaph:get_prop(neo4j_url),
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
