-module(vision_ws_handler).
-behaviour(cowboy_websocket_handler).
-export([
  init/3,websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
]).



-record(ws_state, {
  % pending_delta_query :: undefined | {from_now, N :: non_neg_integer()}, % N events from now
  delta_pid
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.



websocket_handle({text, <<"subscribe_to_instance ", Rest/binary>>}, Req, State) ->
  {ok, State1} = subscribe_to_instance(jsx:decode(Rest, [return_maps]), State),
  {ok, Req, State1};

websocket_handle(Data, Req, State) ->
  lager:error("Unknown websocket message: ~p", [Data]),
  {ok, Req, State}.



websocket_info({delta_query_result, Result}, Req, #ws_state{} = State) ->
  Reply = {text, [<<"delta ">>, jsx:encode(Result)]},
  {reply, Reply, Req, State};

websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.



subscribe_to_instance(#{<<"id">> := Id}, State) ->
  {ok, DeltaPid} = vision_delta_producer:start_link(self(), Id),
  DeltaPid ! {delta_query, chunk_from_now},
  State1 = State#ws_state{delta_pid = DeltaPid},
  {ok, State1}.
