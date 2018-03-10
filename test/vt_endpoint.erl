-module(vt_endpoint).
-export([
  received_from_probe/1, received_from_probe/2, sent_to_probe/1, sent_to_probe/2,
  subscribe_to_session/1,
  subscribe_to_first_guest/0,

  ws_connect/0, ws_send/3, ws_delivered/2
]).



received_from_probe(Atom) -> received_from_probe(Atom, 5000).
received_from_probe(Atom, Timeout) ->
  receive
    {from_probe, {Atom, _} = Message} -> Message;
    {from_probe, {request, _, Atom, _} = Message} -> Message;
    {from_probe, {response, _, Atom, _} = Message} -> Message
  after Timeout ->
    ct:pal("messages ~p", [erlang:process_info(self(), messages)]),
    error(from_probe_message_timeout)
  end.

sent_to_probe(Atom) -> sent_to_probe(Atom, 5000).
sent_to_probe(Atom, Timeout) ->
  receive
    {to_probe, {Atom, _} = Message} -> Message;
    {to_probe, {request, _, Atom, _} = Message} -> Message;
    {to_probe, {response, _, Atom, _} = Message} -> Message
  after Timeout ->
    ct:pal("messages ~p", [erlang:process_info(self(), messages)]),
    error(to_probe_message_timeout)
  end.



subscribe_to_session(#{user_id := UserId}) ->
  EndpointNode = vt:endpoint_node(),
  ok = rpc:call(EndpointNode, vision_test, subscribe_to_session, [self(), #{user_id => UserId}]),
  ok.



subscribe_to_first_guest() ->
  EndpointNode = vt:endpoint_node(),
  ok = rpc:call(EndpointNode, vision_test, subscribe_to_first_guest, [self()]),
  ok.



ws_connect() ->
  application:ensure_all_started(gun),

  {ok, BaseUrl} = application:get_env(vision_test, endpoint_base_url),
  {ok, {http, _, Host, Port, _, _}} = http_uri:parse(BaseUrl),
  {ok, Pid} = gun:open(binary_to_list(Host), Port),
  gun:ws_upgrade(Pid, "/websocket"),

  receive
    {gun_ws_upgrade, Pid, ok, _Headers} -> {ok, Pid};

    {gun_response, Pid, _, _, Status, Headers} ->
      error({ws_upgrade_failed, Status, Headers});
    {gun_error, Pid, _StreamRef, Reason} ->
      error({ws_upgrade_failed, Reason})

  after 1000 ->
    ct:pal("messages: ~p", [erlang:process_info(self(), messages)]),
    exit(failed_to_connect_to_websocket)
  end.



ws_send(Pid, subscribe_to_instance, InstanceId) ->
  Body = iolist_to_binary([<<"subscribe-to-instance ">>, jsx:encode(#{id => InstanceId})]),
  gun:ws_send(Pid, {text, Body}),
  ok;

ws_send(_Pid, Method, Arg) ->
  error({unknown_ws_method, Method, Arg}).


ws_delivered(Pid, Method) ->
  receive
    {gun_ws, Pid, Frame} ->
      ct:pal("got frame: ~p", [Frame]),
      ok
  after 5000 ->
    ct:pal("messages: ~p", [erlang:process_info(self(), messages)]),
    error(timeout_receiving_ws_frame)
  end.
