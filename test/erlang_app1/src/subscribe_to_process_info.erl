-module(subscribe_to_process_info).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  spawn(fun loop/0),
  spawn(fun loop/0),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<>>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.



loop() ->
  timer:sleep(500),
  loop().
