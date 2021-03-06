-module(hello_world).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  lager:info("request started"),
  {ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Hello world!">>, Req),
  Parent = self(),
  spawn(fun () ->
    lager:info("this is log in spawned child, my parent is ~p", [Parent])
  end),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
