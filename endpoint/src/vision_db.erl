-module(vision_db).
-export([query/2]).



% HACK:
% connect to database only on request
% to avoil problems when dropping in webapp test container
%
% should find a way to manage those migrations
query(Query, Fun) ->
  application:ensure_all_started(epgpool),

  {ok, Q} = application:get_env(vision, pg_queries),
  {ok, SQL} = eql:get_query(Query, Q),
  epgpool:with(fun(C) -> Fun(C, SQL) end).