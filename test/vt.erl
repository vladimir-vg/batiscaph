-module(vt).
-export([start_docker_container/3, stop_docker_container/1]).



% We start docker containers and then connect to each other using erlang internode communication.
% Works if all containers run on same machine.
% Some info here: http://erlang.org/pipermail/erlang-questions/2016-May/089259.html



start_docker_container(NodeName, Image, Opts) ->
  {ok, Args} = docker_cmd_args_env(Image, Opts),
  {ok, DockerPath} = find_docker_executable(),

  {ok, DockerContainer} = vt_container:start(DockerPath, Args, NodeName),
  {ok, DockerContainer}.



find_docker_executable() ->
  case os:find_executable("docker") of
    false -> {error, unable_to_locate_docker_executable};
    Path when is_list(Path) -> {ok, Path}
  end.

% all binary keys treated as ENV variables
docker_cmd_args_env(Image, Opts) ->
  Args1 = maps:fold(fun
    (bind_ports, Ports, Args) -> bind_ports_args(Ports) ++ Args;
    (Key, Value, Args) when is_binary(Key) -> env_args(Key, Value) ++ Args
  end, [], Opts),
  Args2 = ["run", "-i"]++Args1++[Image, "/bin/bash"],
  {ok, Args2}.

bind_ports_args(Ports) ->
  lists:concat([["-p", to_binary(P)] || P <- Ports]).

env_args(Key, Value) ->
  ["--env", iolist_to_binary([Key, "=", to_binary(Value)])].




stop_docker_container(Container) ->
  ok = vt_container:stop(Container),
  ok.



to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value).