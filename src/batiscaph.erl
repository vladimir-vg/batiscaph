-module(batiscaph).
-export([get_prop/1, create_tables/0, drop_tables/0, prepare_graph_schema/0]).
-export([binary_to_hex/1]).



create_tables() ->
  ok = clk_events:create_table(),
  ok.

drop_tables() ->
  ok = clk_events:drop_table(),
  ok.

prepare_graph_schema() ->
  {ok, _} = neo4j:commit([
    % NODE KEY constraints are available only in neo4j enterprise edition
    % use concatenated .key property for uniqueness check
    {"CREATE CONSTRAINT ON (p:Process) ASSERT p.key IS UNIQUE", #{}}
  ]),
  ok.



get_prop(Name) ->
  case erlang:get({app_config_prop, Name}) of
    undefined ->
      {ok, Value} = application:get_env(batiscaph, Name),
      put({app_config_prop, Name}, Value),
      Value;
    Value -> Value
  end.



binary_to_hex(Bin) when is_binary(Bin) ->
  << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.