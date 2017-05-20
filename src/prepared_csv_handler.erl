-module(prepared_csv_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  case cowboy_req:path(Req) of
    {<<"/prepared/learn-you-some-erlang/", Filename/binary>>, Req1} ->
      {ok, Body} = read_csv_file(<<"learn-you-some-erlang">>, Filename),
      {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/csv">>}], Body, Req1),
      {ok, Req2, State};

    {_, Req1} ->
      {ok, Req2} = cowboy_req:reply(404, [], <<"Unknown samples dir">>, Req1),
      {ok, Req2, State}
  end.



read_csv_file(Dir, Filename) ->
  PrivDir = code:priv_dir(erltv),
  Path = iolist_to_binary([PrivDir, "/", Dir, "/", Filename, ".repl.csv"]),
  {ok, Body} = file:read_file(Path),
  {ok, Body}.
