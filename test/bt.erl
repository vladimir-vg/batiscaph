-module(bt).
-export([
  test_endpoint_url/0,
  ws_connect/0, ws_send/3, ws_receive/2, ws_receive/3
]).



test_endpoint_url() ->
  {ok, Port} = application:get_env(batiscaph, http_port),
  <<"http://0.0.0.0:", (integer_to_binary(Port))/binary, "/probe">>.



ws_connect() ->
  {ok, Port} = application:get_env(batiscaph, http_port),
  Url = <<"ws://0.0.0.0:", (integer_to_binary(Port))/binary, "/websocket">>,
  {ok, Pid} = bt_ws_client:start_link(Url),
  Pid ! {subscribe, self()},
  {ok, Pid}.



ws_send(Pid, Verb, JSON) ->
  Body = iolist_to_binary([
    atom_to_binary(Verb, latin1),
    <<" ">>,
    jsx:encode(JSON)
  ]),
  websocket_client:cast(Pid, {text, Body}),
  ok.



ws_receive(Pid, Verb) ->
  ws_receive(Pid, Verb, 1000).

ws_receive(Pid, Verb, Timeout) when is_atom(Verb) ->
  ws_receive(Pid, atom_to_binary(Verb, latin1), Timeout);

ws_receive(_Pid, Verb, Timeout) when is_binary(Verb) ->
  receive
    {bt_ws, Verb, Data} -> {ok, Data}

  after Timeout ->
    {error, timeout}
  end.
