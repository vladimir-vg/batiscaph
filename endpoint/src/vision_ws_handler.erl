-module(vision_ws_handler).
-behaviour(cowboy_websocket_handler).
-export([
  init/3, websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
]).



-record(ws_state, {
  % pending_delta_query :: undefined | {from_now, N :: non_neg_integer()}, % N events from now
  % delta_pid
}).



init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #ws_state{}}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.



websocket_handle({text, <<"subscribe_to_instance ", Rest/binary>>}, Req, State) ->
  subscribe_to_instance(jsx:decode(Rest, [return_maps]), Req, State);

websocket_handle({text, <<"connect_to_shell ", Rest/binary>>}, Req, State) ->
  connect_to_shell(jsx:decode(Rest, [return_maps]), Req, State);

websocket_handle({text, <<"shell_input ", Rest/binary>>}, Req, State) ->
  shell_input(jsx:decode(Rest, [return_maps]), Req, State);

websocket_handle({text, <<"subscribe_to_process_info ", Rest/binary>>}, Req, State) ->
  subscribe_to_process_info(jsx:decode(Rest, [return_maps]), Req, State);

websocket_handle({text, <<"unsubscribe_from_process_info ", Rest/binary>>}, Req, State) ->
  unsubscribe_from_process_info(jsx:decode(Rest, [return_maps]), Req, State);

websocket_handle(Data, Req, State) ->
  lager:error("Unknown websocket message: ~p", [Data]),
  {ok, Req, State}.



websocket_info({delta_query_result, Result}, Req, #ws_state{} = State) ->
  Reply = {text, [<<"delta ">>, jsx:encode(Result)]},
  {reply, Reply, Req, State};

websocket_info(Msg, Req, State) ->
  lager:error("Unknown message: ~p", [Msg]),
  {ok, Req, State}.



subscribe_to_instance(#{<<"id">> := Id}, Req, State) ->
  ok = vision_delta_producer:subscribe_to_delta(Id),
  {ok, Req, State}.



connect_to_shell(#{<<"id">> := Id}, Req, State) ->
  case gen_tracker:find(probes, Id) of
    undefined -> {ok, Req, State}; % do nothing for now, TODO: report about disconnect
    {ok, Pid} ->
      % shell might be already existing
      % then we will just receive all previous shell events
      {ok, _} = vision_probe_protocol:remote_request(Pid, ensure_shell_started, []),
      {reply, {text, <<"connected_to_shell">>}, Req, State}
  end.



shell_input(#{<<"id">> := Id, <<"text">> := Text}, Req, State) ->
  case gen_tracker:find(probes, Id) of
    undefined -> {ok, Req, State}; % do nothing if disconnected
    {ok, Pid} ->
      % input is read by shell only after newline
      % so add one
      Text1 = <<Text/binary, "\n">>,
      ok = vision_probe_protocol:send_to_remote(Pid, {shell_input, Text1}),
      {ok, Req, State}
  end.



subscribe_to_process_info(#{<<"instance_id">> := Id, <<"pid">> := Pid}, Req, State) ->
  ok = vision_delta_producer:subscribe_to_process_info(Id, Pid),
  {ok, Req, State}.

unsubscribe_from_process_info(#{<<"instance_id">> := Id, <<"pid">> := Pid}, Req, State) ->
  ok = vision_delta_producer:unsubscribe_from_process_info(Id, Pid),
  {ok, Req, State}.

