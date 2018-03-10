-module(vt_endpoint).
-export([
  received_from_probe/1, received_from_probe/2, sent_to_probe/1, sent_to_probe/2,
  subscribe_to_session/1,
  subscribe_to_first_guest/0
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
