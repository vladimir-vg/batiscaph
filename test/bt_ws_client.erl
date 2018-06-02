-module(bt_ws_client).
-behaviour(websocket_client_handler).
-export([start_link/1]).
-export([
  init/2,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).



start_link(Url) ->
  application:ensure_all_started(ssl),
  websocket_client:start_link(Url, ?MODULE, []).



init([], _ConnState) ->
  {ok, #{subs => []}}.



websocket_handle({pong, _}, _ConnState, State) ->
  {ok, State};

websocket_handle({text, Msg}, _ConnState, #{subs := Subs} = State) ->
  {Verb, Data} = parse_message(Msg),
  [P ! {bt_ws, Verb, Data} || P <- Subs],
  {ok, State}.



websocket_info({subscribe, Pid}, _ConnState, #{subs := Subs} = State) ->
  State1 = State#{subs => [Pid | Subs]},
  {ok, State1}.



websocket_terminate(_CloseInfo, _ConnState, _State) ->
  ok.



parse_message(Msg) ->
  case binary:split(Msg, <<" ">>) of
    [Verb] -> {Verb, none};
    [Verb, Body] -> {Verb, jsx:decode(Body, [return_maps])}
  end.
