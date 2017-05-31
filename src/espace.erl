-module(espace).
-export([start/0, restart/0]).
-export([trace_repl_scenarios/1]).



start() ->
  application:ensure_all_started(espace),
  es_web:restart_cowboy(),
  ok.

restart() ->
  es_web:restart_cowboy(),
  ok.



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

  io:format("found scenarios to play~n~s\n\n", [[["\t", F, "\n"] || F <- ReplFiles1]]),
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
  end,
  exit(Pid, finished), % just in case
  ok.



repl_worker(Parent, Ref, Name) ->
  Self = self(), 
  {ok, CollectorPid} = es_collector:start_link(Name ++ ".csv"),
  {ok, IoServerPid} = es_shell_io_server:start_link(#{collector => CollectorPid, parent => Self, stale_timeout => 5000}),
  {ok, ShellPid} = es_shell_runner:start_link(CollectorPid),

  % capture all stdin/stdout io for shell runner process and its children
  group_leader(IoServerPid, ShellPid),
  {ok, Binary} = file:read_file(Name),

  ok = gen_server:call(CollectorPid, {ignore_pids_tracing, [self(), CollectorPid, IoServerPid, ShellPid]}),
  ok = gen_server:call(ShellPid, start_tracing),

  ShellPid ! restart_shell,
  IoServerPid ! {input, binary_to_list(Binary)},

  erlang:send_after(10000, self(), total_timeout),
  Status = loop_until_finished(IoServerPid, ShellPid),

  ok = wait_until_collector_processed_messages(CollectorPid),

  timer:sleep(100), % TODO: wait until all events processed and written down
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



wait_until_collector_processed_messages(CollectorPid) ->
  case process_info(CollectorPid, message_queue_len) of
    {message_queue_len, 0} -> ok;
    _ -> timer:sleep(100), wait_until_collector_processed_messages(CollectorPid)
  end.