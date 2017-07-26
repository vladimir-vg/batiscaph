-module(es_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).



-record(ws_state, {
  scenario_id,
  local_node_port,
  remote_node,
  remote_scenario_pid
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.



websocket_handle({text, <<"start_shell">>}, Req, #ws_state{scenario_id = undefined} = State) ->
  Id = bin_to_hex:bin_to_hex(crypto:strong_rand_bytes(10)),
  {ok, State1} = start_local_node(State#ws_state{scenario_id = Id}),
  {ok, State2} = start_remote_shell(State1),
  true = gproc:reg({n, l, {websocket, Id}}),
  {reply, {text, <<"shell_started ", Id/binary>>}, Req, State2};

websocket_handle({text, <<"shell_input ", Input/binary>>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! {shell_input, Input},
  % gproc:send({n, l, {erunner, Id}}, {shell_input, Input}),
  {ok, Req, State};

websocket_handle({text, <<"shell_restart">>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! shell_restart,
  % gproc:send({n, l, {erunner, Id}}, shell_restart),
  {ok, Req, State};

websocket_handle({text, <<"store_module ", Rest/binary>>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  [Name, Body] = binary:split(Rest, <<"\n">>),
  ScenarioPid ! {store_module, Name, Body},
  % gproc:send({n, l, {erunner, Id}}, {store_module, Name, Body}),
  {ok, Req, State};

websocket_handle({text, Msg}, Req, State) ->
  lager:error("Unknown websocket command: ~p", [Msg]),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.



websocket_info({events, Events}, Req, State) ->
  JSON = jsx:encode(Events),
  {reply, {text, <<"events ", JSON/binary>>}, Req, State};

websocket_info({store_module, Name, Body}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! {store_module, Name, Body},
  {ok, Req, State};

websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.



websocket_terminate(_Reason, _Req, _State) ->
  ok.



start_local_node(#ws_state{scenario_id = Id} = State) ->
  DirPath = filename:join([code:priv_dir(espace), "scenarios", Id]),
  ok = filelib:ensure_dir(DirPath),
  ok = file:make_dir(DirPath),
  Opts = [{args, ["-noshell", "-sname", Id]}, {cd, DirPath}],
  Port = erlang:open_port({spawn_executable, erl_exec_path()}, Opts),

  RemoteNode = list_to_atom(binary_to_list(Id) ++ "@" ++ net_adm:localhost()),
  ok = wait_for_remote_node(RemoteNode, 5000),
  {ok, State#ws_state{local_node_port = Port, remote_node = RemoteNode}}.

erl_exec_path() ->
  case os:find_executable("erl") of
    false -> "/usr/bin/erl";
    ErlPath when is_list(ErlPath) -> ErlPath
  end.

wait_for_remote_node(_RemoteNode, Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_remote_node(RemoteNode, Timeout) ->
  case net_adm:ping(RemoteNode) of
    pong -> ok;
    pang ->
      timer:sleep(100),
      wait_for_remote_node(RemoteNode, Timeout - 100)
  end.



start_remote_shell(#ws_state{remote_node = RemoteNode, scenario_id = Id} = State) ->
  {ok, ReceiverPid} = remote_events_receiver:start_link(Id, self()),
  ok = remote_node:load_local_module(RemoteNode, bin_to_hex),
  ok = remote_node:load_local_module(RemoteNode, z__remote_collector),
  ok = remote_node:load_local_module(RemoteNode, z__remote_io_server),
  ok = remote_node:load_local_module(RemoteNode, z__remote_scenario),
  ok = remote_node:load_local_module(RemoteNode, z__remote_shell),
  ok = remote_node:load_local_module(RemoteNode, z__remote_sup),
  Opts = #{nodestop_on_disconnect => true, nodestop_on_scenario_shutdown => true},
  {ok, ScenarioPid} = rpc:call(RemoteNode, z__remote_scenario, start_link, [node(), ReceiverPid, Opts]),
  lager:info("Connected to remote shell on ~p", [RemoteNode]),
  {ok, State#ws_state{remote_scenario_pid = ScenarioPid}}.
