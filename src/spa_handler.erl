-module(spa_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Type, Req, []) ->
  {ok, Req, no_state}.



handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/html">>}
  ], <<"<h1>Hello world!</h1>">>, Req),
  {ok, Req2, State}.



terminate(_Reason, _Req, _State) -> ok.

