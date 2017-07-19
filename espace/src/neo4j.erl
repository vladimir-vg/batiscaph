-module(neo4j).
-export([create_index/2]).
-define(TIMEOUT, 5000).



-spec create_index(binary(), [binary()]) -> ok.
create_index(Label, Props) ->
  BaseURL = espace:get_prop(neo4j_url),
  URL = <<BaseURL/binary, "db/data/schema/index/", Label/binary>>,
  Headers = [{<<"Accept">>, <<"application/json; charset=UTF-8">>}, {<<"Content-Type">>, <<"application/json">>}],
  Body = jsx:encode(#{<<"property_keys">> => Props}),

  case hackney:post(URL, Headers, Body, [{timeout, ?TIMEOUT}]) of
    {ok, 409, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> {error, conflict};
        {ok, RespBody} when is_binary(RespBody) ->
          JSON = jsx:decode(RespBody, [return_maps]),
          {error, {conflict, JSON}}
      end;

    {ok, 200, _RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> {error, empty_body};
        {ok, RespBody} when is_binary(RespBody) ->
          JSON = jsx:decode(RespBody, [return_maps]),
          #{<<"label">> := Label, <<"property_keys">> := [_|_]} = JSON,
          ok
      end
  end.

% use Cypher API for other queries
% <<BaseURL/binary, "db/data/cypher">>