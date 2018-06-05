-module(bt_container).
-behaviour(gen_server).
-export([start_link/2]). % supervisor callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks



-record(container, {
  image_name :: binary(),
  opts :: map(),
  port :: port(),
  log_file :: pid()
}).



start_link(ImageName, Opts) ->
  gen_server:start_link(?MODULE, [ImageName, Opts], []).



init([ImageName, Opts]) ->
  self() ! init,
  State = #container{image_name = ImageName, opts = Opts},
  {ok, State}.



handle_info(init, State) ->
  {ok, State1} = start_docker_container(State),
  {noreply, State1};

handle_info({Port, {data, {noeol, Data}}}, #container{port = Port, log_file = LogFile} = State) ->
  ok = file:write(LogFile, Data),
  {noreply, State};

handle_info({Port, {data, {eol, Data}}}, #container{port = Port, log_file = LogFile} = State) ->
  ok = file:write(LogFile, [Data, "\n"]),
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%
%
%



start_docker_container(#container{image_name = ImageName, opts = Opts} = State) ->
  {ok, Args} = docker_cmd_args_env(ImageName, Opts),
  {ok, DockerPath} = find_docker_executable(),
  Port = erlang:open_port({spawn_executable, DockerPath}, [
    {args, Args}, binary, {line, 256}
  ]),

  #{name := Name0, logdir := LogDir} = Opts,
  {ok, LogFile} = file:open(<<LogDir/binary, Name0/binary, ".log">>, [write]),
  State1 = State#container{port = Port, log_file = LogFile},
  {ok, State1}.



find_docker_executable() ->
  case os:find_executable("docker") of
    false -> {error, unable_to_locate_docker_executable};
    Path when is_list(Path) -> {ok, Path}
  end.

docker_cmd_args_env(Image, Opts) ->
  {Args1, Cmd1} = maps:fold(fun
    % these opts do not change command arguments, just skip
    (logdir, _, {Args, Cmd}) -> {Args, Cmd};
    (name, _, {Args, Cmd}) -> {Args, Cmd};
    (host_network, true, {Args, Cmd}) -> {["--net=host" | Args], Cmd};
    (cmd, Cmd, {Args, _}) when is_binary(Cmd) -> {Args, [Cmd]};
    (cmd, Cmd, {Args, _}) when is_list(Cmd) -> {Args, Cmd};

    (Key, Value, {Args, Cmd}) when is_binary(Key) -> {env_args(Key, Value) ++ Args, Cmd}
  end, {[], []}, Opts),
  Args2 = ["run", "-i"]++Args1++[Image] ++ Cmd1, % , "/bin/bash"
  {ok, Args2}.

env_args(Key, Value) ->
  ["--env", iolist_to_binary([Key, "=", to_binary(Value)])].



to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, latin1);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value).
