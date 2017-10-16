-module(batiscaph_steps).
-export([exec/5]).



-record(steps, {
  local_fun_handler,
  func_atom,
  bindings,
  exprs
}).



%%% convenient wrapper around erl_eval
%%% to be used in common test steps execution





% executes expressons step by step
exec(FuncAtom, CtConfig, Bindings, LocalFunFinder, Exprs) ->
  PrivDir = proplists:get_value(priv_dir, CtConfig),
  [_Priv, _RunDir, _, TopRunDir | _] = lists:reverse(filename:split(PrivDir)), % use RunDir as an Id for this ct run
  BatiscaphNode = list_to_atom("batiscaph@" ++ net_adm:localhost()),

  {ok, _} = ct_rpc:call(BatiscaphNode, remote_ctl, ensure_started, [list_to_binary(TopRunDir), node()]),
  ok = wait_for_collector_to_appear(300),

  z__client_scenario:trace_pid(self()),

  LocalFunHandler = fun (Name, Args) ->
    case LocalFunFinder(Name, length(Args)) of
      none -> exit(batiscaph_no_function_found);
      {ok, Func} -> erlang:apply(Func, Args)
    end
  end,

  State = #steps{
    local_fun_handler = LocalFunHandler, func_atom = FuncAtom,
    bindings = Bindings, exprs = Exprs
  },
  exec1(State).



wait_for_collector_to_appear(Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_collector_to_appear(Timeout) ->
  case whereis(z__client_collector) of
    undefined -> timer:sleep(2), wait_for_collector_to_appear(Timeout-2);
    Pid when is_pid(Pid) -> ok
  end.



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
