-module(batiscaph_steps).
-export([exec/5]).



-record(steps, {
  local_fun_handler,
  func_atom,
  bindings,
  exprs,
  receiver_pid
}).



%%% convenient wrapper around erl_eval
%%% to be used in common test steps execution





% executes expressons step by step
exec(FuncAtom, CtConfig, Bindings, LocalFunFinder, Exprs) ->
  % {ok, _} = application:ensure_all_started(batiscaph),
  {ok, ReceiverPid} = batiscaph_receiver:start_link(FuncAtom),
  unlink(ReceiverPid),

  PrivDir = proplists:get_value(priv_dir, CtConfig),

  LocalFunHandler = fun (Name, Args) ->
    case LocalFunFinder(Name, length(Args)) of
      none -> exit(batiscaph_no_function_found);
      {ok, Func} -> erlang:apply(Func, Args)
    end
  end,

  State = #steps{
    local_fun_handler = LocalFunHandler, func_atom = FuncAtom,
    bindings = Bindings, exprs = Exprs, receiver_pid = ReceiverPid,
    priv_dir = PrivDir
  },
  exec1(State).



exec1(#steps{bindings = Bindings, local_fun_handler = LocalFunHandler, exprs = [E]}) ->
  io:format("bindings: ~p~n", [Bindings]),
  {value, Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
  io:format("bindings: ~p~n", [Bindings1]),
  io:format("final value: ~p~n", [Value]),
  Value;

exec1(#steps{bindings = Bindings, local_fun_handler = LocalFunHandler, exprs = [E | Exprs]} = State) ->
  io:format("bindings: ~p~n", [Bindings]),
  {value, _Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
  exec1(State#steps{exprs = Exprs, bindings = Bindings1}).



non_local_function_handler(Func, Args) when is_function(Func) -> erlang:apply(Func, Args);
non_local_function_handler({Module, Atom}, Args) -> erlang:apply(Module, Atom, Args).
