-module(tree_testcases).
-compile(export_all).



% code in this module going to be executed on remote test nodes



spawn_process() ->
  spawn(fun () -> ok end),
  ok.



exit_with_reason() ->
  spawn(fun () -> exit(reason1) end),
  ok.
