-module(delta_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([context_events_create_context/1]).



% This suite start batiscaph, sends list of events
% and checks that resulting delta is correct



all() ->
  [context_events_create_context].



init_per_suite(Config) ->
  % do expect that all necessary ENV variables were specified in advance
  {ok, _} = application:ensure_all_started(batiscaph),
  catch batiscaph:drop_tables(),
  ok = batiscaph:create_tables(),
  Config.

end_per_suite(Config) ->
  
  Config.



%%% give link to neo4j url with query that selects problem graph
%%% print events from clickhouse



context_events_create_context(_Config) ->
  Delta = produce_delta(with_map(#{at_s => g(at_s), pid => g(pid), context => <<"test1">>, instance_id => g(instance_id, ?FUNCTION_NAME)}, [
    #{at_mcs => 1, type => <<"context_start">>},
    #{at_mcs => 2, type => <<"context_stop">>}
  ])),
  #{contexts := [#{context := <<"test1">>, startedAt := _, stoppedAt := _}]} = Delta,
  ok.



%%% helper functions



produce_delta([#{instance_id := Id} | _] = Events) ->
  Events1 = atom_keys_to_binaries(Events),
  {ok, Pid} = remote_ctl:start_link(Id, #{node => false}),
  ok = gen_server:call(Pid, {events, Events1}),
  {ok, Delta} = remote_ctl:delta_json(Id, 0),
  binary_keys_to_atoms(Delta).

atom_keys_to_binaries([]) -> [];
atom_keys_to_binaries([E | Events]) ->
  E1 = maps:fold(fun
    (K, V, Acc) when is_atom(K) -> Acc#{atom_to_binary(K, latin1) => V};
    (K, V, Acc) -> Acc#{K => V}
  end, #{}, E),
  [E1 | atom_keys_to_binaries(Events)].



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



% same list of maps, but with merged pairs from first argument
with_map(Map, List) ->
  [maps:merge(Map, E) || E <- List].
