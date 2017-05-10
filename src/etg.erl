-module(etg). % erlang trace generator
-mode(compile).
-export([trace_repl_scenarios/1]).



% expected that all .repl and .erl files are located in current dir
% .csv files gonna be written here too
trace_repl_scenarios(Dirs) ->
  lists:foreach(fun (Dir) ->
    shell_default:cd(Dir),
    ok = trace_repl_scenarios_dir(),
    shell_default:cd("..")
  end, Dirs),
  ok.

trace_repl_scenarios_dir() ->
  {ok, Filenames} = file:list_dir("."),
  ReplFiles = lists:filter(fun (Name) ->
    case re:run(Name, ".*\\.repl$") of
      nomatch -> false;
      {match, _} -> true
    end
  end, Filenames),
  ReplFiles1 = lists:sort(ReplFiles),

  io:format("found scenarious to play~n~s\n\n", [[["\t", F, "\n"] || F <- ReplFiles1]]),
  [execute_repl_file(Name) || Name <- ReplFiles1],

  ok.



execute_repl_file(Name) ->
  Self = self(),
  Ref = erlang:make_ref(),
  proc_lib:spawn(fun () ->
    repl_worker(Self, Ref, Name)
  end),

  io:format("executing '~s'", [Name]),
  receive
    {Ref, finished} ->
      io:format(", ok\n"),
      ok
  after 10000 ->
    io:format(", timeout\n"),
    {error, timeout}
  end.



repl_worker(Parent, Ref, Name) ->
  {ok, CollectorPid} = etg_collector:start_link(Name ++ ".csv"),
  {ok, IoServerPid} = etg_shell_io_server:start_link(CollectorPid),
  {ok, ShellPid} = etg_shell_runner:start_link(),

  % capture all stdin/stdout io for shell runner process and its children
  group_leader(IoServerPid, ShellPid),
  ShellPid ! start_shell,

  {ok, Binary} = file:read_file(Name),
  IoServerPid ! {input, binary_to_list(Binary)},

  sleep_until_input_consumed(IoServerPid),
  timer:sleep(500),
  Parent ! {Ref, finished},

  ok.



sleep_until_input_consumed(IoServerPid) ->
  case gen_server:call(IoServerPid, pending_input) of
    not_pending -> timer:sleep(100), sleep_until_input_consumed(IoServerPid);
    {pending, Input} ->
      case re:run(Input, "^\s*$") of
        nomatch -> timer:sleep(100), sleep_until_input_consumed(IoServerPid);
        {match, _} -> ok
      end
  end.