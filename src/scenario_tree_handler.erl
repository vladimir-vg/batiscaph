-module(scenario_tree_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  {<<"/scenarios2/", Path/binary>>, Req1} = cowboy_req:path(Req),
  JSON = n4j_processes:delta_json(#{instance_id => Path}),
  Body = jsx:encode(JSON),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req1),
  {ok, Req2, State}.



