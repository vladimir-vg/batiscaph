-module(batiscaph_steps).
-export([exec_testcase/7]).



-record(steps, {
  local_fun_handler,
  testcase,
  context :: binary(),
  bindings,
  exprs
}).



%%% step by step execution of common test testcases
%%% with tracing and reporting about local variables



% executes expressons step by step
exec_testcase(Suite, Testcase, Lines, CtConfig, Bindings, LocalFunFinder, Exprs) ->
  PrivDir = proplists:get_value(priv_dir, CtConfig),
  [_Priv, _RunDir, _, TopRunDir | _] = lists:reverse(filename:split(PrivDir)), % use RunDir as an Id for this ct run
  {ok, BatiscaphNode} = get_batiscaph_node(),

  Context = get_context_path(Suite, Testcase, CtConfig),

  {ok, _} = ct_rpc:call(BatiscaphNode, remote_ctl, ensure_started, [list_to_binary(TopRunDir), #{node => node()}]),
  ok = wait_for_collector_to_appear(300),

  z__client_scenario:trace_pid(self(), #{set_on_spawn => true}),

  LocalFunHandler = fun (Name, Args) ->
    case LocalFunFinder(Name, length(Args)) of
      none -> exit(batiscaph_no_function_found);
      {ok, Func} -> erlang:apply(Func, Args)
    end
  end,

  State = #steps{
    local_fun_handler = LocalFunHandler, testcase = Testcase,
    bindings = Bindings, exprs = Exprs, context = Context
  },
  z__client_collector ! context_start_event(Context, Lines),
  Timestamp = erlang:system_time(micro_seconds),
  ok = log_bindings(Timestamp, Bindings, Context),
  ok = trace_binded_pids(Bindings),

  Value = exec1(State),
  z__client_collector ! context_stop_event(Context),
  Value.



get_batiscaph_node() ->
  case application:get_env(batiscaph, batiscaph_node) of
    {ok, Node} when is_atom(Node) -> {ok, Node};
    undefined ->
      case os:getenv("BATISCAPH_NODE") of
        false -> {error, no_batiscaph_node_to_connect};
        Node when is_list(Node) -> {ok, list_to_atom(Node)}
      end
  end.



wait_for_collector_to_appear(Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_collector_to_appear(Timeout) ->
  case whereis(z__client_collector) of
    undefined -> timer:sleep(2), wait_for_collector_to_appear(Timeout-2);
    Pid when is_pid(Pid) -> ok
  end.




% {tc_group_properties,[{name,group1},parallel,{suite,showcases_SUITE}]},
% {tc_group_path,[[{suite,showcases_SUITE}]]},
% 
% {tc_group_properties,[{suite,showcases_SUITE}]},
% {tc_group_path,[]},
% 
% {tc_group_properties,[{name,group2},{suite,showcases_SUITE}]},
% {tc_group_path,[[{suite,showcases_SUITE}]]},
% 
% {tc_group_properties,[{name,group1_nested},{suite,showcases_SUITE}]},
% {tc_group_path,[[{name,group1},parallel,{suite,showcases_SUITE}],
%                 [{suite,showcases_SUITE}]]},
%
% should return following:
% [suite, group1, group2, ..., testcase]
-spec get_context_path(atom(), atom(), any()) -> binary().
get_context_path(Suite, Testcase, CtConfig) ->
  Props = proplists:get_value(tc_group_properties, CtConfig),
  Path = proplists:get_value(tc_group_path, CtConfig),
  Groups = lists:reverse(lists:flatten(get_group(Props) ++ [get_group(Part) || Part <- Path])),
  ContextAtoms = [Suite] ++ Groups ++ [Testcase],
  iolist_to_binary(lists:join(<<" ">>, [atom_to_binary(A,latin1) || A <- ContextAtoms])).

get_group(Part) ->
  case proplists:get_value(name, Part, undefined) of
    undefined -> [];
    Group when is_atom(Group) -> [Group]
  end.



exec1(#steps{bindings = Bindings, local_fun_handler = LocalFunHandler, exprs = [E], context = Context}) ->
  StartEvalEvent = expr_eval_start_event(erlang:system_time(micro_seconds), E, Context),
  {value, Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
  Timestamp = erlang:system_time(micro_seconds),
  StopEvalEvent = expr_eval_stop_event(Timestamp, E, Context, Value),
  NewBindings = changes_bindings(Bindings, Bindings1),

  ok = log_bindings(Timestamp, NewBindings, Context),
  ok = trace_binded_pids(NewBindings),
  z__client_collector ! {events, [StartEvalEvent, StopEvalEvent]},
  % io:format("final value: ~p~n", [Value]),
  Value;

exec1(#steps{bindings = Bindings, local_fun_handler = LocalFunHandler, exprs = [E | Exprs], context = Context} = State) ->
  StartEvalEvent = expr_eval_start_event(erlang:system_time(micro_seconds), E, Context),
  {value, Value, Bindings1} = erl_eval:expr(E, Bindings, {value, LocalFunHandler}, {value, fun non_local_function_handler/2}),
  Timestamp = erlang:system_time(micro_seconds),
  StopEvalEvent = expr_eval_stop_event(Timestamp, E, Context, Value),
  NewBindings = changes_bindings(Bindings, Bindings1),

  ok = log_bindings(Timestamp, NewBindings, Context),
  ok = trace_binded_pids(NewBindings),
  z__client_collector ! {events, [StartEvalEvent, StopEvalEvent]},
  exec1(State#steps{exprs = Exprs, bindings = Bindings1}).



non_local_function_handler(Func, Args) when is_function(Func) -> erlang:apply(Func, Args);
non_local_function_handler({Module, Atom}, Args) -> erlang:apply(Module, Atom, Args).



changes_bindings(OldBindings, NewBindings) ->
  OldVars = lists:map(fun ({K,_}) -> K end, OldBindings),
  lists:filter(fun ({K,_}) ->
    not lists:member(K, OldVars)
  end, NewBindings).

log_bindings(_Timestamp, [], _Context) -> ok;
log_bindings(Timestamp, Bindings, Context) ->
  BindEvents = [var_bind_event_event(Timestamp, Var, Value, Context) || {Var, Value} <- Bindings],
  VarMentionEvents = [var_mention_events(Timestamp, Var, Value, Context) || {Var, Value} <- Bindings],
  z__client_collector ! {events, BindEvents},
  z__client_collector ! {events, lists:flatten(VarMentionEvents)},
  ok.



context_start_event(Context, Lines) ->
  z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"context_start">>,
    <<"context">> => Context,
    <<"lines">> => erlang:term_to_binary([[N, L] || {N, L} <- Lines])
  }).

context_stop_event(Context) ->
  z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"context_stop">>,
    <<"context">> => Context
  }).

var_bind_event_event(Timestamp, Var, Value, Context) ->
  z__client_collector:event_with_timestamp(Timestamp, #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"var_bind">>,
    <<"context">> => Context,
    <<"atom">> => atom_to_binary(Var, latin1),
    <<"term">> => io_lib:format("~p", [Value])
  }).



trace_binded_pids(Value)
when is_atom(Value) orelse is_number(Value) orelse is_binary(Value)
orelse is_reference(Value) orelse is_port(Value) orelse is_function(Value) ->
  ok;

trace_binded_pids(Value) when is_pid(Value) ->
  ok = z__client_scenario:trace_pid(Value),
  ok;

trace_binded_pids(Value) when is_list(Value) ->
  lists:foreach(fun trace_binded_pids/1, Value),
  ok;

trace_binded_pids(Value) when is_tuple(Value) ->
  Indexes = lists:seq(1, tuple_size(Value)),
  lists:foreach(fun (I) ->
    Value1 = element(I, Value),
    trace_binded_pids(Value1)
  end, Indexes),
  ok;

trace_binded_pids(Value) when is_map(Value) ->
  no_acc = maps:fold(fun (K, V, no_acc) ->
    ok = trace_binded_pids(K),
    ok = trace_binded_pids(V),
    no_acc
  end, no_acc, Value),
  ok.



% walk term recursively, when encounter pid, issue an event of var_mention
% properly construct expression using which this pid can be accessed from testcase
%
% TODO: display pid expr with record syntax, when it possible
var_mention_events(Timestamp, Var, Value, Context) ->
  var_mention_events0(Timestamp, {<<>>, atom_to_binary(Var, latin1), <<>>}, Value, Context).

var_mention_events0(_, _, Value, _) when is_number(Value) -> [];
var_mention_events0(_, _, Value, _) when is_binary(Value) -> [];
var_mention_events0(_, _, Value, _) when is_atom(Value) -> [];
var_mention_events0(_, _, Value, _) when is_reference(Value) -> [];
var_mention_events0(_, _, Value, _) when is_port(Value) -> [];
var_mention_events0(_, _, Value, _) when is_function(Value) -> [];

var_mention_events0(Timestamp, {Prefix, Var, Suffix}, Value, Context) when is_pid(Value) ->
  Expr = <<Prefix/binary, Var/binary, Suffix/binary>>,
  [var_mention_event1(Timestamp, Expr, Value, Context)];

var_mention_events0(Timestamp, {Prefix, Var, Suffix}, Value, Context) when is_tuple(Value) ->
  Indexes = lists:seq(1, tuple_size(Value)),
  lists:map(fun (I) ->
    Prefix1 = <<"element(", (integer_to_binary(I))/binary, ",", Prefix/binary>>,
    Suffix1 = <<Suffix/binary, ")">>,
    Value1 = element(I, Value),
    var_mention_events0(Timestamp, {Prefix1, Var, Suffix1}, Value1, Context)
  end, Indexes);

var_mention_events0(Timestamp, {Prefix, Var, Suffix}, Value, Context) when is_list(Value) ->
  Indexes = lists:seq(1, length(Value)),
  lists:map(fun
    ({I, {Key, Value1}}) ->
      case proplists:get_value(Key, Value) of
        undefined ->
          Prefix1 = <<"lists:nth(", (integer_to_binary(I))/binary, ",", Prefix/binary>>,
          Suffix1 = <<Suffix/binary, ")">>,
          var_mention_events0(Timestamp, {Prefix1, Var, Suffix1}, Value1, Context);

        Value1 ->
          Prefix1 = <<"proplists:get_value(", (list_to_binary(io_lib:format("~p",[Key])))/binary, ",", Prefix/binary>>,
          Suffix1 = <<Suffix/binary, ")">>,
          var_mention_events0(Timestamp, {Prefix1, Var, Suffix1}, Value1, Context)
      end;

    ({I, Value1}) ->
      Prefix1 = <<"lists:nth(", (integer_to_binary(I))/binary, ",", Prefix/binary>>,
      Suffix1 = <<Suffix/binary, ")">>,
      var_mention_events0(Timestamp, {Prefix1, Var, Suffix1}, Value1, Context)
  end, lists:zip(Indexes, Value));

var_mention_events0(Timestamp, {Prefix, Var, Suffix}, Value, Context) when is_map(Value) ->
  Keys = maps:keys(Value),
  lists:map(fun (Key) ->
    Value1 = maps:get(Key, Value),
    Prefix1 = <<"maps:get(", (list_to_binary(io_lib:format("~p",[Key])))/binary, ",", Prefix/binary>>,
    Suffix1 = <<Suffix/binary, ")">>,
    var_mention_events0(Timestamp, {Prefix1, Var, Suffix1}, Value1, Context)
  end, Keys).



var_mention_event1(Timestamp, Expr, Pid, Context) when is_binary(Expr) ->
  z__client_collector:event_with_timestamp(Timestamp, #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"var_mention">>,
    % <<"atom">> => atom_to_binary(Var, latin1),
    <<"term">> => Expr, % this is not really a term, but an expression how this value was extracted
    <<"pid1">> => pid_to_list(Pid),
    <<"context">> => Context
  }).



expr_eval_start_event(Timestamp, Expr, Context) ->
  z__client_collector:event_with_timestamp(Timestamp, #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"expr_eval_start">>,
    <<"term">> => io_lib:format("~p", [Expr]),
    <<"context">> => Context,
    <<"line">> => element(2, Expr)
  }).

expr_eval_stop_event(Timestamp, Expr, Context, Result) ->
  z__client_collector:event_with_timestamp(Timestamp, #{
    <<"pid">> => pid_to_list(self()),
    <<"type">> => <<"expr_eval_stop">>,
    <<"term">> => io_lib:format("~p", [Expr]),
    <<"context">> => Context,
    <<"line">> => element(2, Expr),
    <<"result">> => io_lib:format("~p", [Result])
  }).

% exec_step_start_event(Expr, Lines, Context) ->
%   z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
%     <<"pid">> => pid_to_list(self()),
%     <<"type">> => <<"exec_step_start">>,
%     <<"context">> => Context,
%     <<"term">> => io_lib:format("~p", [Expr])
%   }).
