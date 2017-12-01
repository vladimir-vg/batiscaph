-module(batiscaph_mobx_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).



-record(ws_state, {
  scenario_id :: binary()
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  Id = batiscaph:binary_to_hex(crypto:strong_rand_bytes(10)),
  {ok, Req, #ws_state{scenario_id = Id}}.



websocket_handle({text, <<"event ", JSONBin/binary>>}, Req, #ws_state{} = State) ->
  JSON = jsx:decode(JSONBin, [return_maps]),
  lager:info("received event: ~p", [JSON]),
  {ok, State1} = handle_mobx_event(JSON, State),
  {ok, Req, State1};

websocket_handle({text, Msg}, Req, State) ->
  lager:error("Unknown websocket command: ~p", [Msg]),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.



websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.



websocket_terminate(_Reason, _Req, _State) ->
  ok.



handle_mobx_event(JSON, #ws_state{scenario_id = Id} = State) ->
  ok = batiscaph_mobx:update_graph(JSON#{<<"scenario_id">> => Id}),
  {ok, State}.
