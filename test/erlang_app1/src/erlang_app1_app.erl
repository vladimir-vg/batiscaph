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

  Table = ets:new(stream_tab, []),
  generate_rows(Table, 1000),

  Dispatch = cowboy_router:compile([
    {'_', [
      % official Cowboy examples
      {"/rest_basic_auth", rest_basic_auth, []},
      {"/rest_hello_world", rest_hello_world, []},
      {"/rest_pastebin/[:paste_id]", rest_pastebin, []},
      {"/rest_stream_response/[:v1]", [{v1, int}], rest_stream_response, Table},

      {"/spawn_process", spawn_process, []},
      {"/subscribe_to_process_info", subscribe_to_process_info, []},
      {"/hello_world", hello_world, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  ok.



generate_rows(_Table, 0) ->
  ok;
generate_rows(Table, N) ->
  ets:insert(Table, {key(), val(), val()}),
  generate_rows(Table, N - 1).

key() -> key(10).
key(N) -> key(<< (random:uniform(26) - 1) >>, N - 1).
key(Acc, 0) -> binary_part(base64:encode(Acc), 0, 8);
key(Acc, N) -> key(<< Acc/binary, (random:uniform(26) - 1) >>, N - 1).
val() -> random:uniform(50).
