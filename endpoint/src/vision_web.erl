-module(vision_web).
-include_lib("kernel/include/file.hrl").

-export([start_cowboy/0]).
-export([setup_access_control_headers/1]).



start_cowboy() ->
  {ok, Port} = application:get_env(vision, http_port),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/probe", vision_handler_probe, []},
      {"/api/instances", vision_res_instances, []},
      {"/api/instances/:instance_id/plug-requests/:req_id", vision_res_plug_requests, []},
      {"/api/instances/:instance_id/cowboy-requests/:req_id", vision_res_cowboy_requests, []},
      {"/websocket", vision_ws_handler, []}
      % {"/lib/[...]", cowboy_static, {priv_dir, batiscaph, "wwwroot/lib", [{mimetypes, cow_mimetypes, all}]}},
      % {"/app/[...]", cowboy_static, {priv_dir, batiscaph, "wwwroot/app", [{mimetypes, cow_mimetypes, all}]}},
      % {"/scenarios.json", scenarios_list_handler, []},
      % % {"/scenarios/[:id]/erlmodules", erlmodules_handler, []},
      % % {"/scenarios/[...]", scenarios_csv_handler, []},
      % % {"/api/scenarios2/[:id]", scenario_tree_handler, []},
      % {"/style/app.css", cowboy_static, {priv_file, batiscaph, "wwwroot/app.css"}},
      % {"/", cowboy_static, {priv_file, batiscaph, "wwwroot/index.html"}},
      % {"/scenarios/[:id]", cowboy_static, {priv_file, batiscaph, "wwwroot/index.html"}},
      % {"/scenarios/[:id]/[...]", cowboy_static, {priv_file, batiscaph, "wwwroot/index.html"}}
    ]}
  ]),
  Opts = [
    {env, [{dispatch, Dispatch}]}
    % {onresponse, fun translate_jsx_if_precompiled_unavailable/4}
  ],
  {ok, _} = cowboy:start_http(vision_http, 5, [{port, Port}], Opts),
  lager:info("Started http server on ~p port", [Port]),

  % % if webserver already running
  % case (catch ranch:get_port(batiscaph_http)) of
  %   Port ->
  %     % just refresh routing and loaded modules
  %     ranch:set_protocol_options(batiscaph_http, Opts),
  %     ok;
  % 
  %   _ ->
  %     ranch:stop_listener(batiscaph_http),
  %     {ok, _} = cowboy:start_http(batiscaph_http, 5, [{port, Port}], Opts),
  %     lager:info("Started http server on ~p port", [Port]),
  %     ok
  % end,
  ok.



setup_access_control_headers(Req) ->
  Origin = list_to_binary(os:getenv("VISION_ENDPOINT_ALLOW_ORIGIN_URL")),
  Req1 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, authorization">>, Req2),
  Req4 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req3),
  {ok, Req4}.
