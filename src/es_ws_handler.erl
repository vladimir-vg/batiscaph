-module(es_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).



-record(ws_state, {
  scenario_id,
  remote_scenario_pid
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.



websocket_handle({text, <<"start_shell">>}, Req, #ws_state{scenario_id = undefined} = State) ->
  {ok, Reply, State1} = start_shell(State),
  {reply, Reply, Req, State1};

websocket_handle({text, <<"connect_to_shell ", Id/binary>>}, Req, #ws_state{scenario_id = undefined} = State) ->
  {ok, Reply, State1} = connect_to_shell(Id, State),
  {reply, Reply, Req, State1};



websocket_handle({text, <<"shell_input ", Input/binary>>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! {shell_input, Input},
  {ok, Req, State};

websocket_handle({text, <<"shell_restart">>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! shell_restart,
  {ok, Req, State};

websocket_handle({text, <<"store_module ", Rest/binary>>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  [Name, Body] = binary:split(Rest, <<"\n">>),
  ScenarioPid ! {store_module, Name, Body},
  {ok, Req, State};

websocket_handle({text, <<"trace_pid ", Pid/binary>>}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
  ScenarioPid ! {trace_pid, Pid},
  {ok, Req, State};

websocket_handle({text, Msg}, Req, State) ->
  lager:error("Unknown websocket command: ~p", [Msg]),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.



websocket_info({delta, Delta}, Req, State) ->
  JSON = jsx:encode(Delta),
  {reply, {text, <<"delta ", JSON/binary>>}, Req, State};

% websocket_info({store_module, Name, Body}, Req, #ws_state{remote_scenario_pid = ScenarioPid} = State) ->
%   ScenarioPid ! {store_module, Name, Body},
%   {ok, Req, State};

websocket_info({shell_input, {ready, Prompt}}, Req, #ws_state{} = State) ->
  {reply, {text, iolist_to_binary(["shell_input_ready ", Prompt])}, Req, State};

websocket_info({shell_input, stopped}, Req, #ws_state{} = State) ->
  {reply, {text, <<"shell_input_stopped">>}, Req, State};

websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.



websocket_terminate(_Reason, _Req, _State) ->
  ok.



start_shell(State) ->
  Id = espace:binary_to_hex(crypto:strong_rand_bytes(10)),
  {ok, Pid} = remote_ctl:ensure_started(Id),
  ok = gen_server:call(Pid, {subscribe_websocket, self()}),
  {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
  State1 = State#ws_state{scenario_id = Id, remote_scenario_pid = ScenarioPid},
  {ok, {text, <<"shell_connected ", Id/binary>>}, State1}.



connect_to_shell(Id, State) ->
  case remote_ctl:currently_running(Id) of
    none ->
      {ok, Delta} = remote_ctl:delta_json(Id, 0),
      self() ! {delta, Delta},
      State1 = State#ws_state{scenario_id = Id},
      {ok, {text, <<"shell_lost ", Id/binary>>}, State1};

    {ok, Pid} ->
      ok = gen_server:call(Pid, {subscribe_websocket, self()}),
      {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
      State1 = State#ws_state{scenario_id = Id, remote_scenario_pid = ScenarioPid},
      {ok, {text, <<"shell_connected ", Id/binary>>}, State1}
  end.


% connect_to_remote_node(#ws_state{remote_node = RemoteNode} = State) ->
%   ok = wait_for_remote_node(RemoteNode, 5000),
%   {ok, State}.



% start_local_node(#ws_state{scenario_id = Id} = State) ->
%   DirPath = filename:join([code:priv_dir(espace), "scenarios", Id]),
%   ok = filelib:ensure_dir(DirPath),
%   ok = file:make_dir(DirPath),
%   Opts = [{args, ["-noshell", "-sname", Id]}, {cd, DirPath}],
%   Port = erlang:open_port({spawn_executable, erl_exec_path()}, Opts),
% 
%   RemoteNode = list_to_atom(binary_to_list(Id) ++ "@" ++ net_adm:localhost()),
%   ok = wait_for_remote_node(RemoteNode, 5000),
%   {ok, State#ws_state{local_node_port = Port, remote_node = RemoteNode}}.
% 
% erl_exec_path() ->
%   case os:find_executable("erl") of
%     false -> "/usr/bin/erl";
%     ErlPath when is_list(ErlPath) -> ErlPath
%   end.
% 
% wait_for_remote_node(_RemoteNode, Timeout) when Timeout =< 0 -> {error, timeout};
% wait_for_remote_node(RemoteNode, Timeout) ->
%   case net_adm:ping(RemoteNode) of
%     pong -> ok;
%     pang ->
%       timer:sleep(100),
%       wait_for_remote_node(RemoteNode, Timeout - 100)
%   end.



% start_remote_shell(#ws_state{remote_node = RemoteNode, scenario_id = Id} = State) ->
%   {ok, ReceiverPid} = remote_ctl:start_link(Id, self()),
%   ok = remote_node:load_local_module(RemoteNode, z__remote_collector),
%   ok = remote_node:load_local_module(RemoteNode, z__remote_io_server),
%   ok = remote_node:load_local_module(RemoteNode, z__remote_scenario),
%   ok = remote_node:load_local_module(RemoteNode, z__remote_shell),
%   ok = remote_node:load_local_module(RemoteNode, z__remote_sup),
%   Opts = #{nodestop_on_disconnect => true, nodestop_on_scenario_shutdown => true},
%   {ok, ScenarioPid} = rpc:call(RemoteNode, z__remote_scenario, start_link, [node(), ReceiverPid, Opts]),
%   lager:info("Connected to remote shell on ~p", [RemoteNode]),
%   {ok, State#ws_state{remote_scenario_pid = ScenarioPid}}.
