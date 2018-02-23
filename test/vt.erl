-module(vt).
-export([
  start_docker_container/3, stop_docker_container/1,
  ensure_endpoint_running/1, endpoint_url/0, endpoint_node/0
]).



% We start docker containers and then connect to each other using erlang internode communication.
% Works if all containers run on same machine.
% Some info here: http://erlang.org/pipermail/erlang-questions/2016-May/089259.html



ensure_endpoint_running(#{logdir := LogDir}) ->
  Port = 8080,
  NodeName = endpoint1,
  DockerName = endpoint1,

  case whereis(endpoint_container) of
    % already started current endpoint
    Pid when is_pid(Pid) -> ok;

    % not started yet, probably old version still running
    undefined ->
      % kill endpoint that still alive from previous test run
      ok = kill_docker_container_with_name(to_binary(DockerName)),
      Opts = #{
        <<"VISION_ENDPOINT_HTTP_PORT">> => Port,
        host_network => true,
        logdir => LogDir,
        docker_name => to_binary(DockerName),
        detached => true
      },
      {ok, EndpointContainer} = start_docker_container(to_binary(NodeName), <<"vision/endpoint:latest">>, Opts),

      EndpointNode = vt_container:node(EndpointContainer),
      EndpointUrl = generate_endpoint_url(EndpointNode, Port),
      application:set_env(vision_test, endpoint_url, EndpointUrl),
      application:set_env(vision_test, endpoint_node, EndpointNode),

      ok = wait_for_application(vt_container:node(EndpointContainer), batiscaph, 5000),

      Pid = vt_container:owner_pid(EndpointContainer),
      register(endpoint_container, Pid),
      ok
  end.

kill_docker_container_with_name(Name) when is_binary(Name) ->
  kill_docker_container_with_name(binary_to_list(Name));

kill_docker_container_with_name(Name) when is_list(Name) ->
  % rename old container, to make name vacant
  % kill it and remove in background
  % allow new container to start
  OldName = "old_container_" ++ Name,
  ct:pal("~s", [os:cmd("docker rename "++Name++" "++OldName)]),
  ct:pal("~s", [os:cmd("docker kill "++OldName)]),
  ct:pal("~s", [os:cmd("docker rm "++OldName)]),
  ok.

generate_endpoint_url(EndpointNode, Port) ->
  Host = hostname_from_node(EndpointNode),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port), "/probe"]).

hostname_from_node(Node) ->
  [_Name, Host] = binary:split(atom_to_binary(Node,latin1), <<"@">>),
  Host.

wait_for_application(_, _, Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_application(Node, App, Timeout) ->
  Apps = rpc:call(Node, application, which_applications, []),
  case lists:keyfind(App, 1, Apps) of
    false -> timer:sleep(100), wait_for_application(Node, App, Timeout-100);
    {App, _Version, _Desc} -> ok
  end.



endpoint_url() ->
  {ok, EndpointUrl} = application:get_env(vision_test, endpoint_url),
  EndpointUrl.

endpoint_node() ->
  {ok, EndpointNode} = application:get_env(vision_test, endpoint_node),
  EndpointNode.



start_docker_container(NodeName, Image, Opts) ->
  {ok, Args} = docker_cmd_args_env(Image, Opts),
  {ok, DockerPath} = find_docker_executable(),

  Opts1 = maps:with([autostart, logdir, before_start, detached], Opts),
  {ok, DockerContainer} = vt_container:start(DockerPath, Args, NodeName, Opts1),
  {ok, DockerContainer}.



find_docker_executable() ->
  case os:find_executable("docker") of
    false -> {error, unable_to_locate_docker_executable};
    Path when is_list(Path) -> {ok, Path}
  end.

% all binary keys treated as ENV variables
docker_cmd_args_env(Image, Opts) ->
  Args1 = maps:fold(fun
    % these opts do not change command arguments, just skip
    (autostart, _, Args) -> Args;
    (logdir, _, Args) -> Args;
    (detached, true, Args) -> ["-d" | Args];
    (host_network, true, Args) -> ["--net=host" | Args];
    (docker_name, Name, Args) -> ["--name", Name | Args];

    (Key, Value, Args) when is_binary(Key) -> env_args(Key, Value) ++ Args
  end, [], Opts),
  Args2 = ["run", "-i"]++Args1++[Image, "/bin/bash"],
  {ok, Args2}.

env_args(Key, Value) ->
  ["--env", iolist_to_binary([Key, "=", to_binary(Value)])].




stop_docker_container(Container) ->
  ok = vt_container:stop(Container),
  ok.



to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, latin1);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value).