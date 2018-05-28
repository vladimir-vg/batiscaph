-module(spawn_process).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<>>, Req),

  spawn(fun () -> ok end),

  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
