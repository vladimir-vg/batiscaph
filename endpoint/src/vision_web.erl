-module(vision_web).
-include_lib("kernel/include/file.hrl").

-export([start_cowboy/0]).



start_cowboy() ->
  {ok, Port} = application:get_env(vision, http_port),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/probe", vision_handler_probe, []},

      {"/api/instances", vision_res_instances, []},
      {"/api/instances/:instance_id/plug-requests/:req_id", vision_res_plug_requests, []},
      {"/api/instances/:instance_id/cowboy-requests/:req_id", vision_res_cowboy_requests, []},
      {"/websocket", vision_ws_handler, []},

      {"/static/[...]", cowboy_static, {priv_dir, vision, "compiled_static", [{mimetypes, cow_mimetypes, all}]}},
      {"/", cowboy_static, {priv_file, vision, "index.html"}},
      {"/instances/[:instance_id]", cowboy_static, {priv_file, vision, "index.html"}},
      {"/instances/[:instance_id]/[...]", cowboy_static, {priv_file, vision, "index.html"}}
    ]}
  ]),
  Opts = [
    {env, [{dispatch, Dispatch}]}
  ],
  {ok, _} = cowboy:start_http(vision_http, 5, [{port, Port}], Opts),
  lager:info("Started http server on ~p port", [Port]),

  ok.

