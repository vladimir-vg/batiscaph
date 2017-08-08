-module(remote_events_receiver).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%%% This process accept events from remote node
%%% and stores it to database. Sends prepared events to websocket.



-record(remote_control, {
  id,
  websocket_pid,
  graph_producer_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id, WebsocketPid) ->
  gen_server:start_link(?MODULE, [Id, WebsocketPid], []).

init([Id, WebsocketPid]) ->
  {ok, ProducerPid} = graph_producer:start_link(Id),
  {ok, #remote_control{id = Id, websocket_pid = WebsocketPid, graph_producer_pid = ProducerPid}}.



handle_info({events, Events}, #remote_control{id = Id, websocket_pid = WebsocketPid, graph_producer_pid = ProducerPid} = State) ->
  WebsocketPid ! {events, Events}, % try to send events if websocket is alive
  ok = es_events:store(Id, Events),
  ProducerPid ! new_events_stored,
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
