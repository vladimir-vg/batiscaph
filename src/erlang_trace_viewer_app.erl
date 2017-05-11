-module(erlang_trace_viewer_app).
-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
  ok = start_cowboy(),
  erlang_trace_viewer_sup:start_link().



stop(_State) ->
  ok.



start_cowboy() ->
  Port = 8099,
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/[...]", spa_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
  lager:info("Started http server on ~p port", [Port]),
  ok.