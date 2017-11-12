-module(bt).
-export([g/1, g/2, binarify_events/1, atomize_delta/1, atomize_events/1, wait_for_collector_to_appear/0]).



% Batiscaph Test utils



% generate function
% create sample value for given event field
g(at_s) ->
  Min = 1400000000,
  Max = 1509115271,
  Min + rand:uniform(Max-Min);

g(pid) ->
  A1 = integer_to_binary(rand:uniform(1000)),
  A2 = integer_to_binary(rand:uniform(1000)),
  A3 = integer_to_binary(rand:uniform(1000)),
  iolist_to_binary(["<",A1,".",A2,".",A3,">"]).

g(instance_id, Testcase) ->
  iolist_to_binary([
    atom_to_binary(Testcase, latin1), "/",
    batiscaph:binary_to_hex(crypto:strong_rand_bytes(5))
  ]).



binarify_events(Events) -> atom_keys_to_binaries(Events).

atom_keys_to_binaries([]) -> [];
atom_keys_to_binaries([E | Events]) ->
  E1 = maps:fold(fun
    (K, V, Acc) when is_atom(K) -> Acc#{atom_to_binary(K, latin1) => V};
    (K, V, Acc) -> Acc#{K => V}
  end, #{}, E),
  [E1 | atom_keys_to_binaries(Events)].



% TODO: do not atomize line numbers and pids as keys
atomize_delta(Delta) -> binary_keys_to_atoms(Delta).

binary_keys_to_atoms(Value) when is_binary(Value) -> Value;
binary_keys_to_atoms(Value) when is_number(Value) -> Value;
binary_keys_to_atoms(Value) when is_atom(Value) -> Value;
binary_keys_to_atoms(List) when is_list(List) ->
  [binary_keys_to_atoms(E) || E <- List];
binary_keys_to_atoms(Map) when is_map(Map) ->
  maps:fold(fun
    (K, V, Acc) when is_binary(K) -> Acc#{binary_to_atom(K, latin1) => binary_keys_to_atoms(V)};
    (K, V, Acc) when is_atom(K) -> Acc#{K => binary_keys_to_atoms(V)}
  end, #{}, Map).



atomize_events([]) -> [];
atomize_events([E | Events]) ->
  E1 = maps:fold(fun
    (K, V, Acc) when is_binary(K) -> Acc#{binary_to_atom(K, latin1) => V};
    (K, V, Acc) -> Acc#{K => V}
  end, #{}, E),
  [E1 | atomize_events(Events)].



wait_for_collector_to_appear() -> wait_for_collector_to_appear(300).

wait_for_collector_to_appear(Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_collector_to_appear(Timeout) ->
  case whereis(z__client_collector) of
    undefined -> timer:sleep(5), wait_for_collector_to_appear(Timeout-5);
    Pid when is_pid(Pid) -> ok
  end.