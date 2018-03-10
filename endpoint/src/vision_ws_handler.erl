-module(vision_ws_handler).
-behaviour(cowboy_websocket_handler).
-export([
  init/3,websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
]).



-record(ws_state, {
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.



websocket_handle(Data, Req, State) ->
  lager:error("Unknown websocket message: ~p", [Data]),
  {ok, Req, State}.



websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.
