-module(old_batiscaph_ws_handler).
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
websocket_handle({text, <<"start_shell_on_node ", Node/binary>>}, Req, #ws_state{scenario_id = undefined} = State) ->
  {ok, Reply, State1} = start_shell(Node, State),
  {reply, Reply, Req, State1};

websocket_handle({text, <<"connect_to_shell ", Rest/binary>>}, Req, #ws_state{scenario_id = undefined} = State) ->
  {ok, Reply, State1} = case binary:split(Rest, <<" ">>) of
    [Id] -> connect_to_shell(Id, undefined, State);
    [Id, Context] -> connect_to_shell(Id, Context, State)
  end,
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

websocket_handle({text, <<"trace_pid ", Pid/binary>>}, Req, #ws_state{remote_scenario_pid = undefined} = State) ->
  lager:info("remote scenario is dead, ignore trace_pid(~s) command", [Pid]),
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
  start_shell(undefined, State).

  % Id = batiscaph:binary_to_hex(crypto:strong_rand_bytes(10)),
  % {ok, Pid} = remote_ctl:ensure_started(Id),
  % ok = gen_server:call(Pid, {subscribe_websocket, self(), #{}}),
  % {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
  % State1 = State#ws_state{scenario_id = Id, remote_scenario_pid = ScenarioPid},
  % {ok, {text, <<"shell_connected ", Id/binary>>}, State1}.

start_shell(Node, State) when is_binary(Node) ->
  start_shell(binary_to_atom(Node, latin1), State);

start_shell(Node, State) ->
  Opts = case Node of
    undefined -> #{};
    _ when is_atom(Node) -> #{node => Node}
  end,
  Id = batiscaph:binary_to_hex(crypto:strong_rand_bytes(10)),
  {ok, Pid} = remote_ctl:ensure_started(Id, Opts),
  ok = gen_server:call(Pid, {subscribe_websocket, self(), #{}}),
  {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
  State1 = State#ws_state{scenario_id = Id, remote_scenario_pid = ScenarioPid},
  {ok, {text, <<"shell_connected ", Id/binary>>}, State1}.



connect_to_shell(Id, Context, State) ->
  QueryOpts = case Context of
    undefined -> #{};
    _ when is_binary(Context) -> #{context => Context}
  end,

  case remote_ctl:currently_running(Id) of
    none ->
      {ok, Delta} = remote_ctl:delta_json(QueryOpts#{instance_id => Id}),
      self() ! {delta, Delta},
      State1 = State#ws_state{scenario_id = Id},
      {ok, {text, <<"shell_lost ", Id/binary>>}, State1};

    {ok, Pid} ->
      ok = gen_server:call(Pid, {subscribe_websocket, self(), QueryOpts}),
      {ok, ScenarioPid} = gen_server:call(Pid, get_scenario_pid),
      State1 = State#ws_state{scenario_id = Id, remote_scenario_pid = ScenarioPid},
      {ok, {text, <<"shell_connected ", Id/binary>>}, State1}
  end.

