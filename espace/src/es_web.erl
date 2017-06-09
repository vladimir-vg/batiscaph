-module(es_web).
-include_lib("kernel/include/file.hrl").

-export([restart_cowboy/0]).



restart_cowboy() ->
  % try create cache table if not created
  % useful to cache babel output
  catch ets:new(web_page_cache, [public, named_table, set]),

  {ok, Port} = application:get_env(espace, http_port),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/api/v0", es_v0_handler, []},
      {"/websocket", es_ws_handler, []},
      {"/vendor/[...]", cowboy_static, {priv_dir, espace, "wwwroot/vendor", [{mimetypes, cow_mimetypes, all}]}},
      {"/app/[...]", cowboy_static, {priv_dir, espace, "wwwroot/app", [{mimetypes, cow_mimetypes, all}]}},
      {"/prepared/[...]", prepared_csv_handler, []},
      {"/style/app.css", cowboy_static, {priv_file, espace, "wwwroot/app.css"}},
      {"/", cowboy_static, {priv_file, espace, "wwwroot/index.html"}}
    ]}
  ]),
  Opts = [
    {env, [{dispatch, Dispatch}]},
    {onresponse, fun translate_jsx_if_precompiled_unavailable/4}
  ],

  % if webserver already running
  case (catch ranch:get_port(espace_http)) of
    Port ->
      % just refresh routing and loaded modules
      ranch:set_protocol_options(espace_http, Opts),
      ok;

    _ ->
      ranch:stop_listener(espace_http),
      {ok, _} = cowboy:start_http(espace_http, 100, [{port, Port}], Opts),
      lager:info("Started http server on ~p port", [Port]),
      ok
  end,
  ok.



translate_jsx_if_precompiled_unavailable(404, Headers, <<>>, Req) ->
  case cowboy_req:path(Req) of
    {<<"/app/", Path/binary>>, Req1} ->
      case find_jsx_for_path(Path) of
        not_found -> Req1; % okay, continue returning 404
        {found, MTime, FilePath} ->
          case fetch_jsx_from_cache(MTime, FilePath) of
            {ok, Body} -> respond_with_body(200, Headers, Body, Req);
            _ ->
              case compile_jsx(FilePath) of
                {ok, Body} ->
                  ok = save_to_jsx_cache(FilePath, MTime, Body),
                  respond_with_body(200, Headers, Body, Req);

                {error, timeout} ->
                  lager:error("timeout during babel jsx translation: /app/~s", [Path]),
                  respond_with_body(500, Headers, <<"timeout during babel jsx translation">>, Req);

                {error, {babel, Body}} ->
                  lager:error("babel translation error:\n~s\n", [Body]),
                  respond_with_body(500, Headers, Body, Req)
              end
          end
      end;

    {_, Req1} -> Req1
  end;

translate_jsx_if_precompiled_unavailable(_Status, _Headers, _Body, Req) -> Req.



find_jsx_for_path(Path) ->
  DirPath = list_to_binary(code:priv_dir(espace)),
  % replace last .js with .jsx
  NoExtSize = byte_size(Path) - 3,
  <<NoExt:NoExtSize/binary, _:3/binary>> = Path,
  FullPath = <<DirPath/binary, "/wwwroot/app/", NoExt/binary, ".jsx">>,
  case file:read_file_info(FullPath) of
    {ok, #file_info{type = regular, mtime = MTime}} -> {found, MTime, FullPath};
    _ -> not_found
  end.



fetch_jsx_from_cache(MTime, Path) ->
  case ets:lookup(web_page_cache, Path) of
    [{Path, MTime, Body}] -> {ok, Body};
    [{Path, MTime1, _Body}] when MTime1 < MTime -> outdated;
    [] -> not_found
  end.

save_to_jsx_cache(Path, MTime, Body) ->
  ets:insert(web_page_cache, {Path, MTime, Body}),
  ok.



compile_jsx(FilePath) ->
  WorkDir = code:priv_dir(espace) ++ "/..",
  Opts = [binary, stream, exit_status, stderr_to_stdout, {cd, WorkDir}],
  Port = erlang:open_port({spawn, <<"./node_modules/.bin/babel --presets es2015,react ", FilePath/binary>>}, Opts),
  compile_jsx_receive_loop(Port, <<>>).

compile_jsx_receive_loop(Port, Acc) ->
  receive
    {Port, {data, Binary}} -> compile_jsx_receive_loop(Port, <<Acc/binary, Binary/binary>>);
    {Port, {exit_status, 0}} -> {ok, Acc};
    {Port, {exit_status, _}} -> {error, {babel, Acc}}
  after 10000 ->
    {error, timeout}
  end.



respond_with_body(Code, Headers, Body, Req) ->
  Headers1 = lists:keyreplace(<<"content-length">>, 1, Headers, {<<"content-length">>, integer_to_list(byte_size(Body))}),
  {ok, Req1} = cowboy_req:reply(Code, Headers1, Body, Req),
  Req1.
