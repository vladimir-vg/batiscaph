-module(es_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).



-record(ws_state, {
  date,
  scenario_id,
  runner_port
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.



websocket_handle({text, <<"start_shell">>}, Req, #ws_state{date = undefined, scenario_id = undefined} = State) ->
  {ok, State1} = start_shell(State),
  #ws_state{date = Dir, scenario_id = Id} = State1,
  {reply, {text, <<"shell_started ", Dir/binary, "/", Id/binary>>}, Req, State1};

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



start_shell(#ws_state{date = undefined, scenario_id = undefined} = State) ->
  {{Year, Month, Day}, _Time} = calendar:local_time(),
  Date = iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w",[Year,Month,Day])),
  Id = bin_to_hex:bin_to_hex(crypto:strong_rand_bytes(10)),

  ets:insert(events_subscribers, {Id, self()}),

  {ok, Port} = erunner_ctl:start(Date, Id),
  {ok, State#ws_state{scenario_id = Id, date = Date, runner_port = Port}}.