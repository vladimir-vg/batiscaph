-module(scenario_tree_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  {Bindings, Req1} = cowboy_req:bindings(Req),
  Id = proplists:get_value(id, Bindings),
  {ok, JSON} = graph_producer:delta_json(Id, 0),
  Body = jsx:encode(JSON),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req1),
  {ok, Req2, State}.



