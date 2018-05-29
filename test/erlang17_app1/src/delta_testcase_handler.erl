-module(delta_testcase_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Type, Req, []) ->
  {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  {TestcaseName, Req1} = cowboy_req:binding(testcase_name, Req),
  FName = binary_to_atom(TestcaseName, latin1),

  T1 = erlang:now(),
  Response =
    try delta_testcases:FName() of
      ok -> ok;
      Result -> {ok, Result}
    catch
      Class:Reason -> {error, Class, Reason}
    end,
  T2 = erlang:now(),

  Body = io_lib:format("~p", [Response]),
  Headers = resp_headers(T1, T2) ++ [{<<"content-type">>, <<"text/plain">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req1),
  {ok, Req2, State}.



resp_headers({Mega1, Sec1, Micro1}, {Mega2, Sec2, Micro2}) ->
  At1 = Mega1*1000*1000*1000*1000 + Sec1*1000*1000 + Micro1,
  At2 = Mega2*1000*1000*1000*1000 + Sec2*1000*1000 + Micro2,
  [
    {<<"x-pid">>, list_to_binary(erlang:pid_to_list(self()))},
    {<<"x-started-at">>, integer_to_binary(At1)},
    {<<"x-stopped-at">>, integer_to_binary(At2)}
  ].
