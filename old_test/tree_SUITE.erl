-module(tree_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  empty_tree/1,
  children_always_to_right_from_parent/1
]).



% This suite takes delta, passes it through objtree.js,
% and check resulting tree (ready for display)



all() ->
  [empty_tree, children_always_to_right_from_parent].



init_per_suite(Config) ->
	case os:find_executable("nodejs") of
    false -> {skip, unable_to_locate_javascript_interpreter};
    Path when is_list(Path) -> [{nodejs, Path} | Config]
  end.

end_per_suite(Config) ->
  Config.



empty_tree(Config) ->
  Delta = #{events => [], contexts => []},
  Tree = apply_delta(Delta, Config),
  #{height := 0, width := 0, contexts := #{}, links := #{}, mentions := #{}, points := #{}, processes := #{}, spawns := #{}}
    = Tree,
  ok.



children_always_to_right_from_parent(Config) ->
  % children should always be placed to the right of their parents
  BaseTime1 = bt:g(at),

  Delta = #{
    processes => #{
      <<"pidA">> => #{
        pid => <<"pidA">>, appearedAt => BaseTime1,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1}]},
      <<"pidA1">> => #{
        pid => <<"pidA1">>, parentPid => <<"pidA">>, appearedAt => BaseTime1+40, spawnedAt => BaseTime1+40,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1+40}]},
      <<"pidA2">> => #{
        pid => <<"pidA2">>, parentPid => <<"pidA">>, appearedAt => BaseTime1+100, spawnedAt => BaseTime1+100,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1+100}]},

      <<"pidB">> => #{
        pid => <<"pidB">>, appearedAt => BaseTime1-100,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1-100}]},
      <<"pidB1">> => #{
        pid => <<"pidB1">>, parentPid => <<"pidB">>, appearedAt => BaseTime1+30, spawnedAt => BaseTime1+30,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1+30}]},
      <<"pidB2">> => #{
        pid => <<"pidB2">>, parentPid => <<"pidB">>, appearedAt => BaseTime1+50, spawnedAt => BaseTime1+50,
        events => [#{type => <<"TRACE_STARTED">>, at => BaseTime1+50}]}
    },
    events => [
      #{at => BaseTime1+30, type => <<"SPAWN">>, pid1 => <<"pidB">>, pid2 => <<"pidB1">>},
      #{at => BaseTime1+40, type => <<"SPAWN">>, pid1 => <<"pidA">>, pid2 => <<"pidA1">>},
      #{at => BaseTime1+50, type => <<"SPAWN">>, pid1 => <<"pidB">>, pid2 => <<"pidB2">>},
      #{at => BaseTime1+100, type => <<"SPAWN">>, pid1 => <<"pidA">>, pid2 => <<"pidA2">>}
    ]
  },

  #{processes := Processes} = apply_delta(Delta, Config),
  #{
    <<"pidA">> := #{x:=PidAX}, <<"pidA1">> := #{x:=PidA1X}, <<"pidA2">> := #{x:=PidA2X},
    <<"pidB">> := #{x:=PidBX}, <<"pidB1">> := #{x:=PidB1X}, <<"pidB2">> := #{x:=PidB2X}
  } = Processes,

  true = PidAX < PidA1X,
  true = PidAX < PidA2X,
  true = PidBX < PidB1X,
  true = PidBX < PidB2X,

  ok.



% helper functions



apply_delta(Delta, Config) ->
  NodeJs = proplists:get_value(nodejs, Config),
  ScriptPath = code:lib_dir(batiscaph) ++ "/test/tree_SUITE.js",
  Opts = [{args, [ScriptPath]}, {cd, code:priv_dir(batiscaph)}, binary],

  Port = erlang:open_port({spawn_executable, NodeJs}, Opts),
  erlang:port_command(Port, jsx:encode(Delta)),
  erlang:port_command(Port, <<"\n\n">>),

  receive
    {Port, {data, Data}} ->
      jsx:decode(Data, [return_maps, {labels, attempt_atom}])
  after 5000 ->
    {error, timeout}
  end.
