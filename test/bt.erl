-module(bt).
-export([
  test_endpoint_url/0,
  ws_connect/0, ws_send/3, ws_receive/2, ws_receive/3,
  match_tree/3
]).



test_endpoint_url() ->
  {ok, Port} = application:get_env(batiscaph, http_port),
  <<"http://0.0.0.0:", (integer_to_binary(Port))/binary, "/probe">>.



ws_connect() ->
  {ok, Port} = application:get_env(batiscaph, http_port),
  Url = <<"ws://0.0.0.0:", (integer_to_binary(Port))/binary, "/websocket">>,
  {ok, Pid} = bt_ws_client:start_link(Url),
  Pid ! {subscribe, self()},
  {ok, Pid}.



ws_send(Pid, Verb, JSON) when is_pid(Pid) ->
  Body = iolist_to_binary([
    atom_to_binary(Verb, latin1),
    <<" ">>,
    jsx:encode(JSON)
  ]),
  websocket_client:cast(Pid, {text, Body}),
  ok.



ws_receive(Pid, Verb) ->
  ws_receive(Pid, Verb, 1000).

ws_receive(Pid, Verb, Timeout) when is_atom(Verb) ->
  ws_receive(Pid, atom_to_binary(Verb, latin1), Timeout);

ws_receive(_Pid, Verb, Timeout) when is_binary(Verb) ->
  receive
    {bt_ws, Verb, none} -> ok;
    {bt_ws, Verb, Data} -> {ok, Data}
  after Timeout ->
    {error, timeout}
  end.



% it tries to match every fresh delta against MatchFunc
% until timeout
match_tree(MatchFunc, MatchState, Opts) ->
  case maps:is_key(timeout, Opts) of
    true -> match_tree1(MatchFunc, MatchState, Opts);
    false -> match_tree1(MatchFunc, MatchState, Opts#{timeout => 10000})
  end.



% already established websocket might be provided in opts
% no need to subscribe
match_tree1(MatchFunc, MatchState, #{websocket_pid := _, timeout := _} = State) ->
  State1 = State#{
    match_started_at => erlang:system_time(milli_seconds)
  },
  start_match_loop(MatchFunc, MatchState, State1);

% websocket is not provieded, need to create and subscribe
match_tree1(MatchFunc, MatchState, #{instance_id := InstanceId, timeout := _} = State) ->
  {ok, WsPid} = bt:ws_connect(),
  ok = bt:ws_send(WsPid, subscribe_to_instance, #{id => InstanceId}),
  State1 = State#{
    match_started_at => erlang:system_time(milli_seconds),
    websocket_pid => WsPid, close_ws_after_match => true
  },
  start_match_loop(MatchFunc, MatchState, State1).



start_match_loop(MatchFunc, MatchState, State) ->
  case get(last_match_tree) of
    undefined -> match_tree_loop(MatchFunc, MatchState, State);
    #{} = Tree ->
      try MatchFunc(Tree, MatchState) of
        {more, MatchState1} -> match_tree_loop(MatchFunc, MatchState1, State#{last_tree => Tree});
        {done, MatchState1} -> finish_match_tree_loop(MatchState1, State#{last_tree => Tree})
      catch
        error:function_clause -> match_tree_loop(MatchFunc, MatchState, State#{last_tree => Tree})
      end
  end.



match_tree_loop(MatchFunc, MatchState, State) ->
  #{match_started_at := StartedAt, timeout := Timeout, websocket_pid := WsPid} = State,
  case erlang:system_time(milli_seconds) - StartedAt of
    TimeSpent when TimeSpent > Timeout -> {error, timeout};
    TimeSpent ->
      TimeLeft = Timeout - TimeSpent,

      case bt:ws_receive(WsPid, delta, TimeLeft) of
        {error, timeout} ->
          ct:pal("match state: ~p", [MatchState]),
          ct:pal("tree: ~p", [maps:get(last_tree, State, undefined)]),
          error(tree_didnt_match);

        {ok, Delta} ->
          % currently we receive whole tree each time instead of deltas
          % in future here should be done a merge with all previously received deltas
          Tree = atomize_delta(Delta),

          try MatchFunc(Tree, MatchState) of
            {more, MatchState1} -> match_tree_loop(MatchFunc, MatchState1, State#{last_tree => Tree});
            {done, MatchState1} -> finish_match_tree_loop(MatchState1, State#{last_tree => Tree})
          catch
            error:function_clause -> match_tree_loop(MatchFunc, MatchState, State#{last_tree => Tree})
          end
      end
  end.



finish_match_tree_loop(MatchState, #{websocket_pid := WsPid, last_tree := Tree} = State) ->
  case maps:get(close_ws_after_match, State, false) of
    false -> ok;
    true ->
      unlink(WsPid),
      exit(WsPid, close_ws_after_match)
  end,

  % for simplicity put it into process dictionary
  erlang:put(last_match_tree, Tree),
  {ok, MatchState}.



atomize_delta(Delta) ->
  % turn first level keys into atoms
  maps:fold(fun (K, V, Acc) ->
    Acc#{binary_to_atom(K,latin1) => V}
  end, #{}, Delta).
