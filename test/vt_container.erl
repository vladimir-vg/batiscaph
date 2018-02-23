-module(vt_container).
-export([start/4, node/1, start_node/1, stop/1]).
-export([loop/1]).

-record(docker_container, {
  node,
  port_owner_pid
}).

-record(container_state, {
  node,
  port,
  started = false,
  logfile,
  stdout_buffer = <<>>
}).



node(#docker_container{node = Node}) -> Node.

start(DockerPath, Args, NodeName, Opts) ->
  #{logdir := LogDir} = Opts,

  Parent = self(),
  % starting unlinked process that going to last forever
  % port dies if spawner dies, so just start something
  ct:pal("~s~n", [iolist_to_binary(lists:join(" ", [DockerPath | Args]))]),
  PortOwner = proc_lib:spawn(fun () ->
    Port = erlang:open_port({spawn_executable, DockerPath}, [{args, Args}, binary, {line, 256}]),
    {ok, Ip} = read_node_ip_address(Port),
    Node = <<NodeName/binary, "@", Ip/binary>>,

    Handle = #docker_container{node = binary_to_atom(Node, latin1), port_owner_pid = self()},
    Parent ! Handle,

    {ok, LogFile} = file:open(<<LogDir/binary, Node/binary, ".log">>, [write]),
    State = #container_state{node = Node, port = Port, logfile = LogFile},
    ?MODULE:loop(State)
  end),
  erlang:monitor(process, PortOwner),

  receive #docker_container{} = DockerNode ->
    case maps:get(autostart, Opts, true) of
      false -> {ok, DockerNode};
      true ->
        % starting neo4j and clickhouse in container might be really slow
        ok = start_node(DockerNode, 30000),
        {ok, DockerNode}
    end

  after 10000 ->
    ct:pal("port owner info: ~p~n", [erlang:process_info(PortOwner)]),
    ct:pal("messages: ~p~n", [erlang:process_info(self(), messages)]),
    error(unable_to_start_docker_container)
  end.



stop(#docker_container{port_owner_pid = Pid}) ->
  Pid ! {stop, self()},
  receive {stopped, Pid} -> ok
  after 5000 -> error(no_response_from_port_owner)
  end.



start_node(State) ->
  start_node(State, 5000).

start_node(#docker_container{port_owner_pid = Pid}, Timeout) ->
  Pid ! {start_node, self()},
  receive {started_node, Pid} -> ok
  after Timeout -> error(no_response_from_port_owner)
  end.



%
% Private code below
%



loop(#container_state{port = Port, node = Node, started = Started, stdout_buffer = Buffer, logfile = LogFile} = State) ->
  receive
    {start_node, From} when Started =:= false ->
      ok = start_node1(Node, Port),
      From ! {started_node, self()},
      ?MODULE:loop(State#container_state{started = true});

    {stop, From} ->
      if Started -> erlang:port_command(Port, <<"init:stop().\n">>);
        true -> ok
      end,
      erlang:port_command(Port, <<"exit\n">>),
      erlang:port_close(Port),
      From ! {stopped, self()},
      ok;

    % collect stdout and print it
    {Port, {data, {noeol, Data}}} ->
      ?MODULE:loop(State#container_state{stdout_buffer = <<Buffer/binary, Data/binary>>});
    {Port, {data, {eol, Data}}} ->
      ok = file:write(LogFile, <<Buffer/binary, Data/binary, "\n">>),
      % ct:pal("~s:\t~s", [Node, ]),
      ?MODULE:loop(State#container_state{stdout_buffer = <<>>})

  after 5000 ->
    ?MODULE:loop(State)
  end.



% parse ip addres out of this:
%
% 33: eth0@if34: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default 
%     link/ether 02:42:ac:13:00:03 brd ff:ff:ff:ff:ff:ff
%     inet 172.19.0.3/16 scope global eth0
%        valid_lft forever preferred_lft forever
%
% {messages,[{#Port<0.52242>,
%             {data,{eol,<<"39: eth0@if40: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default ">>}}},
%            {#Port<0.52242>,
%             {data,{eol,<<"    link/ether 02:42:ac:13:00:04 brd ff:ff:ff:ff:ff:ff">>}}},
%            {#Port<0.52242>,
%             {data,{eol,<<"    inet 172.19.0.4/16 scope global eth0">>}}},
%            {#Port<0.52242>,
%             {data,{eol,<<"       valid_lft forever preferred_lft forever">>}}}]}
read_node_ip_address(Port) ->
  Port ! {self(), {command, <<"ip addr show dev docker0\n">>}},

  % skip two lines, take third one
  receive {Port, {data, {eol, _}}} -> ok after 1000 -> error(no_output_from_node) end,
  receive {Port, {data, {eol, _}}} -> ok after 1000 -> error(no_output_from_node) end,
  {ok, Rest} =
    receive {Port, {data, {eol, <<"    inet ", Rest1/binary>>}}} -> {ok, Rest1}
    after 1000 -> error(no_output_from_node)
    end,

  % just read out the rest from mailbox
  ok = clear_port_output(Port),

  [Ip1, _] = binary:split(Rest, <<" ">>),
  [Ip2, _] = binary:split(Ip1, <<"/">>),
  {ok, Ip2}.



start_node1(Node, Port) ->
  Port ! {self(), {command, <<"./rebar3 shell --name ", Node/binary, " --setcookie vision-test\n">>}},
  % wait until common test Erlang node would successfully connects to just started node
  ok = wait_for_node(binary_to_atom(Node, latin1), 2000),
  ok = clear_port_output(Port),
  ok.

% just read out all output messages for this port
% to not pollute mailbox, make it not leak
clear_port_output(Port) ->
  receive {Port, {data, _}} -> clear_port_output(Port)
  after 0 -> ok
  end.

wait_for_node(_Node, Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_node(Node, Timeout) ->
  case net_adm:ping(Node) of
    pong -> ok;
    pang ->
      timer:sleep(10),
      wait_for_node(Node, Timeout-10)
  end.
