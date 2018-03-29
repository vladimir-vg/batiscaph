-module(spawn_process).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	io:format("response init ~p\n", [erlang:trace_info(self(), flags)]),
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
	], <<"Hello world!">>, Req),
	spawn(fun () -> ok end),
	io:format("response sent ~p\n", [erlang:trace_info(self(), flags)]),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
