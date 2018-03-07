-module(vt).
-export([
  ensure_started/1,

  start_docker_container/3, stop_docker_container/1,
  endpoint_url/0, endpoint_node/0,
  webapp_node/0,

  base_url/2,
  api_request/3,

  received_from_probe/1, received_from_probe/2, sent_to_probe/1, sent_to_probe/2
]).



% We start docker containers and then connect to each other using erlang internode communication.
% Works if all containers run on same machine.
% Some info here: http://erlang.org/pipermail/erlang-questions/2016-May/089259.html


ensure_started(#{logdir := LogDir}) ->
  case {whereis(webapp_container), whereis(endpoint_container)} of
    % already started current endpoint
    {Pid1, Pid2} when is_pid(Pid1) andalso is_pid(Pid2) -> ok;
    _ ->
      % postgresql databases are not possble to drop
      % if someone still connected to it
      % that's why we should stop all containers first
      % and only then start them (and run migrations)
      ok = kill_docker_container_with_name(<<"endpoint1">>),
      ok = kill_docker_container_with_name(<<"web1">>),

      % webapp takes url to endpoint as API url
      % need to start endpoint first, to determine that url
      ok = run_fresh_endpoint(<<"endpoint1">>, LogDir),
      ok = run_fresh_webapp(<<"web1">>, LogDir)
  end,
  ok.



run_fresh_endpoint(DockerName, LogDir) ->
  Port = 8081,
  NodeName = '_endpoint1', % added underscore just to appear on top when sorted by name
  Opts = #{
    <<"VISION_ENDPOINT_HTTP_PORT">> => Port,
    <<"VISION_ENDPOINT_POSTGRES_URL">> => <<"postgres://postgres:postgres@127.0.0.1/vision_test">>,
    host_network => true,
    logdir => LogDir,
    docker_name => DockerName,
    detached => true
  },
  {ok, EndpointContainer} = start_docker_container(NodeName, <<"vision/endpoint:latest">>, Opts),

  EndpointUrl = generate_endpoint_url(EndpointContainer, Port),
  EndpointBaseUrl = base_url(EndpointContainer, Port),
  EndpointNode = vt_container:node(EndpointContainer),
  application:set_env(vision_test, endpoint_url, EndpointUrl),
  application:set_env(vision_test, endpoint_base_url, EndpointBaseUrl),
  application:set_env(vision_test, endpoint_node, EndpointNode),

  ok = wait_for_application(vt_container:node(EndpointContainer), vision, 5000),

  Pid = vt_container:owner_pid(EndpointContainer),
  register(endpoint_container, Pid),
  ok.

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



generate_endpoint_url(EndpointContainer, Port) ->
  <<(base_url(EndpointContainer, Port))/binary, "/probe">>.

host_from_node(Node) ->
  [_Name, Host] = binary:split(atom_to_binary(Node,latin1), <<"@">>),
  Host.

base_url(Container, Port) ->
  Node = vt_container:node(Container),
  Host = host_from_node(Node),
  iolist_to_binary(["http://", Host, ":", integer_to_binary(Port)]).



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



run_fresh_webapp(DockerName, LogDir) ->
  Port = 8082,
  NodeName = '_web1',

  {ok, EndpointBaseUrl} = application:get_env(vision_test, endpoint_base_url),

  Opts = #{
    <<"VISION_WEB_POSTGRES_URL">> => <<"postgres://postgres:postgres@127.0.0.1/vision_test">>,
    <<"VISION_WEB_HTTP_PORT">> => Port,
    <<"VISION_WEB_API_URL">> => <<EndpointBaseUrl/binary, "/api">>,
    host_network => true,
    logdir => LogDir,
    docker_name => DockerName,
    detached => true,
    runtype => phoenix
  },
  {ok, WebContainer} = start_docker_container(NodeName, <<"vision/web:latest">>, Opts),

  WebappNode = vt_container:node(WebContainer),
  application:set_env(vision_test, webapp_node, WebappNode),

  ok = wait_for_application(vt_container:node(WebContainer), vision, 5000),

  Pid = vt_container:owner_pid(WebContainer),
  register(webapp_container, Pid),
  ok.

webapp_node() ->
  {ok, WebappNode} = application:get_env(vision_test, webapp_node),
  WebappNode.



start_docker_container(NodeName, Image, Opts) when is_atom(NodeName) ->
  start_docker_container(atom_to_binary(NodeName, latin1), Image, Opts);

start_docker_container(NodeName, Image, Opts) when is_binary(NodeName) ->
  {ok, Args} = docker_cmd_args_env(Image, Opts),
  {ok, DockerPath} = find_docker_executable(),

  Opts1 = maps:with([autostart, logdir, before_start, detached, runtype], Opts),
  {ok, DockerContainer} = vt_container:start(DockerPath, Args, NodeName, Opts1),

  case maps:get(wait_for_application, Opts, undefined) of
    undefined -> {ok, DockerContainer};
    AppAtom ->
      ok = wait_for_application(vt_container:node(DockerContainer), AppAtom, 5000),
      {ok, DockerContainer}
  end.



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
    (runtype, _, Args) -> Args;
    (wait_for_application, _, Args) -> Args;
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



api_request(Method, Resource, Arg) when is_atom(Resource) ->
  api_request(Method, atom_to_binary(Resource,latin1), Arg);

api_request(get, Resource, Arg) when is_binary(Resource) andalso is_list(Arg) ->
  {ok, BaseUrl} = application:get_env(vision_test, endpoint_base_url),
  Url1 = hackney_url:make_url(BaseUrl, [<<"api">>, Resource], to_qs_vals(Arg)),
  {ok, 200, _RespHeaders, Body} = hackney:request(get, Url1, [], <<>>, [with_body]),
  {ok, jsx:decode(Body, [return_maps])}.

to_qs_vals([]) -> [];
to_qs_vals([{K,V} | Rest]) ->
  [{to_binary(K), to_binary(V)} | to_qs_vals(Rest)].



to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, latin1);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value).



received_from_probe(Atom) -> received_from_probe(Atom, 5000).
received_from_probe(Atom, Timeout) ->
  receive
    {from_probe, {Atom, _} = Message} -> Message;
    {from_probe, {request, _, Atom, _} = Message} -> Message;
    {from_probe, {response, _, Atom, _} = Message} -> Message
  after Timeout ->
    ct:pal("messages ~p", [erlang:process_info(self(), messages)]),
    error(from_probe_message_timeout)
  end.

sent_to_probe(Atom) -> sent_to_probe(Atom, 5000).
sent_to_probe(Atom, Timeout) ->
  receive
    {to_probe, {Atom, _} = Message} -> Message;
    {to_probe, {request, _, Atom, _} = Message} -> Message;
    {to_probe, {response, _, Atom, _} = Message} -> Message
  after Timeout ->
    ct:pal("messages ~p", [erlang:process_info(self(), messages)]),
    error(to_probe_message_timeout)
  end.
