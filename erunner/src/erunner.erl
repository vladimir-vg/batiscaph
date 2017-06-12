-module(erunner).
-export([start/0]).



start() ->
  MasterPort = list_to_integer(os:getenv("MASTER_PORT")),
  Id = list_to_binary(os:getenv("SHELL_SESSION_ID")),

  {ok, Pid} = erunner_scenario:start_link(MasterPort, Id),

  % if gen_server died then stop whole node
  unlink(Pid),
  MRef = erlang:monitor(process, Pid),
  receive
    {'DOWN', MRef, process, Pid, Reason} ->
      io:format("Slave ~p died due to ~p\n", [Id, Reason]),
      timer:sleep(1000),
      init:stop()
  end,

  ok.
