-module(espace_shell).
-export([steps_exec/3]).



%%% convenient wrapper around erl_eval
%%% to be used in common test steps execution



% executes expressons step by step
steps_exec(_Bindings, _LocalFunHandler, _Exprs) ->
  % case LocalFunHandler(local_fun1, 2) of
  %   {ok, Fun} -> io:format("found function: ~p~n", [Fun(foo, [a,b])]);
  %   none -> io:format("found no function =(\n")
  % end,
  ok.