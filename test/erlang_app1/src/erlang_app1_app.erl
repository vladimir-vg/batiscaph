-module(erlang_app1_app).
-behaviour(application).
-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
  case os:getenv("HTTP_PORT") of
    false -> ok;
    Port -> start_cowboy(list_to_integer(Port))
  end,
  erlang_app1_sup:start_link().

stop(_State) ->
  ok.



start_cowboy(Port) ->
  application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/spawn_process", spawn_process, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  ok.
