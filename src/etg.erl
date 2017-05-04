-module(etg). % erlang trace generator
-mode(compile).
-export([main/0]).


main() ->
  {ok, CollectorPid} = etg_collector:start_link(<<"/tmp/trace_events.csv">>),
  {ok, IoServerPid} = etg_shell_io_server:start_link(CollectorPid),
  {ok, ShellPid} = etg_shell_runner:start_link(),

  % capture all stdin/stdout io for shell runner process and its children
  group_leader(IoServerPid, ShellPid),
  ShellPid ! start_shell,

  IoServerPid ! {input, "X = 123, Y = 5455, X + Y.\n"},
  IoServerPid ! {input, "X + Y + 33333.\n"},
  timer:sleep(1000),

  ok.