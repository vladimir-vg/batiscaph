-module(batiscaph_web).
-include_lib("kernel/include/file.hrl").

-export([start_cowboy/0]).



start_cowboy() ->
  {ok, Port} = application:get_env(batiscaph, http_port),

  % if freshly compiled js is available, then serve it
  % otherwise serve js compiled for the last stable release
  StaticDir = case filelib:is_dir(code:priv_dir(batiscaph) ++ "/compiled_static") of
    true -> "compiled_static";
    false -> "release_static"
  end,

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/probe", batiscaph_handler_probe, []},

      {"/api/instances", batiscaph_res_instances, []},
      {"/api/instances/:instance_id/plug_requests/:req_id", batiscaph_res_plug_requests, []},
      {"/api/instances/:instance_id/cowboy_requests/:req_id", batiscaph_res_cowboy_requests, []},
      {"/websocket", batiscaph_ws_handler, []},

      {"/static/[...]", cowboy_static, {priv_dir, batiscaph, StaticDir, [{mimetypes, cow_mimetypes, all}]}},
      {"/", cowboy_static, {priv_file, batiscaph, "index.html"}},
      {"/instances/[:instance_id]", cowboy_static, {priv_file, batiscaph, "index.html"}},
      {"/instances/[:instance_id]/[...]", cowboy_static, {priv_file, batiscaph, "index.html"}}
    ]}
  ]),
  Opts = [
    {env, [{dispatch, Dispatch}]}
  ],
  {ok, _} = cowboy:start_http(batiscaph_http, 5, [{port, Port}], Opts),
  lager:info("Started http server on ~p port", [Port]),

  ok.

