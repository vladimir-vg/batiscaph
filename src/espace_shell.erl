-module(espace_shell).
-export([steps_exec/3]).



%%% convenient wrapper around erl_eval
%%% to be used in common test steps execution



% executes expressons step by step
steps_exec(Bindings, LocalFunFinder, Exprs) ->
  LocalFunHandler = fun (Name, Args) ->
    case LocalFunFinder(Name, length(Args)) of
      none -> exit(espace_no_function_found);
      {ok, Func} -> erlang:apply(Func, Args)
    end
  end,
  steps_exec1(Bindings, LocalFunHandler, Exprs).



steps_exec1(Bindings, LocalFunHandler, [E]) ->
  io:format("bindings: ~p~n", [Bindings]),
  {value, Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
    io:format("bindings: ~p~n", [Bindings1]),
    io:format("final value: ~p~n", [Value]),
  Value;
steps_exec1(Bindings, LocalFunHandler, [E | Exprs]) ->
  io:format("bindings: ~p~n", [Bindings]),
  {value, _Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
  steps_exec1(Bindings1, LocalFunHandler, Exprs).



non_local_function_handler(Func, Args) when is_function(Func) -> erlang:apply(Func, Args);
non_local_function_handler({Module, Atom}, Args) -> erlang:apply(Module, Atom, Args).
  