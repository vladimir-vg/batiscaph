-module(batiscaph_mobx).
-export([update_graph/1]).



update_graph(Event) ->
  Statements = neo4j_statements_from_event(Event),
  {ok, _} = neo4j:commit(Statements),
  ok.



neo4j_statements_from_event(#{}) ->
  [].
