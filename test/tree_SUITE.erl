-module(tree_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([empty_tree/1]).



% This suite takes delta, passes it through objtree.js,
% and check resulting tree (ready for display)



all() ->
  [empty_tree].



init_per_suite(Config) ->
	case os:find_executable("nodejs") of
    false -> {skip, unable_to_locate_javascript_interpreter};
    Path when is_list(Path) -> [{nodejs, Path} | Config]
  end.

end_per_suite(Config) ->
  Config.



empty_tree(Config) ->
  Delta = #{graph_events => [], table_events => [], contexts => []},
  Tree = apply_delta(Delta, Config),
  #{height := 0, width := 0, contexts := #{}, links := #{}, mentions := #{}, points := #{}, processes := #{}, spawns := #{}}
    = Tree,
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
