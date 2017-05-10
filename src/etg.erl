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
  Pid = proc_lib:spawn(fun () ->
    repl_worker(Self, Ref, Name)
  end),
  MRef = erlang:monitor(process, Pid),

  io:format("executing '~s'", [Name]),
  receive
    {'DOWN', MRef, process, Pid, Reason} ->
      io:format(", dead: ~p\n", [Reason]),
      ok;
    {Ref, finished, Status} ->
      io:format(", ~s\n", [Status]),
      ok
  end.



repl_worker(Parent, Ref, Name) ->
  Self = self(), 
  {ok, CollectorPid} = etg_collector:start_link(Name ++ ".csv"),
  {ok, IoServerPid} = etg_shell_io_server:start_link(#{collector => CollectorPid, parent => Self, stale_timeout => 5000}),
  {ok, ShellPid} = etg_shell_runner:start_link(),

  % capture all stdin/stdout io for shell runner process and its children
  group_leader(IoServerPid, ShellPid),
  ShellPid ! restart_shell,

  {ok, Binary} = file:read_file(Name),
  IoServerPid ! {input, binary_to_list(Binary)},

  erlang:send_after(10000, self(), total_timeout),
  Status = loop_until_finished(IoServerPid, ShellPid),
  timer:sleep(500),
  Parent ! {Ref, finished, Status},

  ok.



loop_until_finished(IoServerPid, ShellPid) ->
  receive
    total_timeout -> timeout;
    {IoServerPid, stalled} ->
      ok = gen_server:call(IoServerPid, clear_pending),
      ShellPid ! restart_shell,
      loop_until_finished(IoServerPid, ShellPid)
  after 0 ->
    case is_everything_consumed(IoServerPid) of
      false -> timer:sleep(100), loop_until_finished(IoServerPid, ShellPid);
      true -> ok
    end
  end.



is_everything_consumed(IoServerPid) ->
  case gen_server:call(IoServerPid, pending_input) of
    not_pending -> false; % looks like busy processing last command
    {pending, Input} ->
      case re:run(Input, "^\s*$") of
        {match, _} -> true; % only whitespace left, exit
        nomatch -> false
      end
  end.

