-module(bt).
-export([
  test_endpoint_url/0,
  ws_connect/0, ws_send/3, ws_receive/2, ws_receive/3
]).



test_endpoint_url() ->
  {ok, Port} = application:get_env(batiscaph, http_port),
  <<"http://0.0.0.0:", (integer_to_binary(Port))/binary, "/probe">>.



ws_connect() ->
  application:ensure_all_started(gun),
  {ok, Port} = application:get_env(batiscaph, http_port),
  {ok, Pid} = gun:open("0.0.0.0", Port),
  gun:ws_upgrade(Pid, "/websocket"),

  receive
    {gun_ws_upgrade, Pid, ok, _Headers} -> {ok, Pid};

    {gun_response, Pid, _, _, Status, Headers} ->
      error({ws_upgrade_failed, Status, Headers});
    {gun_error, Pid, _StreamRef, Reason} ->
      error({ws_upgrade_failed, Reason})

  after 1000 ->
    exit(failed_to_connect_to_websocket)
  end.



ws_send(Pid, Verb, JSON) ->
  Body = iolist_to_binary([
    atom_to_binary(Verb, latin1),
    <<" ">>,
    jsx:encode(JSON)
  ]),
  gun:ws_send(Pid, {text, Body}),
  ok.



ws_receive(Pid, Verb) ->
  ws_receive(Pid, Verb, 1000).

ws_receive(Pid, Verb, Timeout) when is_atom(Verb) ->
  ws_receive(Pid, atom_to_binary(Verb, latin1), Timeout);

ws_receive(Pid, Verb, Timeout) when is_binary(Verb) ->
  Length = byte_size(Verb),
  receive
    {gun_ws, Pid, {text, <<Verb:Length/binary, " ", Payload/binary>>}} ->
      {ok, jsx:decode(Payload, [return_maps])};

    {gun_ws, Pid, {text, <<Verb:Length/binary>>}} ->
      erlang:binary_to_atom(Verb, latin1)

    % {gun_ws, Pid, Frame} ->
    %   ct:pal("got unknown frame: ~p", [Frame]),
    %   {error, Frame}

  after Timeout ->
    {error, timeout}
  end.
