-module(remote_espace_scenario).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ESPACE_NODE_TIMEOUT, 5000).



%%% This module does node-to-node communication



-record(scenario, {
  opts,
  espace_node,
  espace_node_timer
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(ESpaceNode, Opts) ->
  gen_server:start_link(?MODULE, [ESpaceNode, Opts], []).

init([ESpaceNode, Opts]) ->
  self() ! init,
  {ok, #scenario{espace_node = ESpaceNode, opts = Opts}}.



handle_info(init, #scenario{opts = Opts} = State) ->
  case maps:get(die_on_node_disconnect, Opts, undefined) of
    undefined -> {noreply, State};
    true ->
      {ok, State1} = refresh_espace_node_timer(State),
      {noreply, State1}
  end;

handle_info(check_espace_node, #scenario{espace_node = ESpaceNode} = State) ->
  ok = check_espace_node(ESpaceNode),
  {ok, State1} = refresh_espace_node_timer(State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



refresh_espace_node_timer(#scenario{espace_node_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  refresh_espace_node_timer(State#scenario{espace_node_timer = undefined});

refresh_espace_node_timer(#scenario{espace_node_timer = undefined} = State) ->
  Timer = erlang:send_after(?ESPACE_NODE_TIMEOUT, self(), check_espace_node),
  {ok, State#scenario{espace_node_timer = Timer}}.



check_espace_node(ESpaceNode) ->
  case net_adm:ping(ESpaceNode) of
    pong -> ok;
    pang ->
      % remote espace is unavailable, shut down node
      io:format("Espace node is gone, shutting down\n"),
      timer:sleep(1000),
      init:stop()
  end.