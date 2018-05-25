-module(batiscaph_probe_lager_backend).
-behaviour(gen_event).

-export([
  init/1, handle_call/2, handle_event/2, handle_info/2,
  terminate/2, code_change/3
]).



init([]) ->
  {ok, no_state}.



handle_call(_Request, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% @private
handle_event({log, Message}, State) ->
  Event = lager_message_to_event(Message),
  ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, [Event]}}),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.



lager_message_to_event(Message) ->
  Metadata = lager_msg:metadata(Message),
  Text = lager_msg:message(Message),
  #{
    at => lager_msg:timestamp(Message),
    type => <<"p1 lager:event">>,
    pid1 => any_to_binary(proplists:get_value(pid, Metadata, <<>>)),
    text => erlang:list_to_binary(Text),
    application => any_to_binary(proplists:get_value(application, Metadata, <<>>)),
    module => any_to_binary(proplists:get_value(module, Metadata, <<>>)),
    function => any_to_binary(proplists:get_value(function, Metadata, <<>>)),
    line => any_to_binary(proplists:get_value(line, Metadata, <<>>))
  }.



any_to_binary(Value) when is_integer(Value) -> erlang:integer_to_binary(Value);
any_to_binary(Value) when is_list(Value) -> erlang:list_to_binary(Value);
any_to_binary(Value) when is_atom(Value) -> erlang:atom_to_binary(Value, latin1);
any_to_binary(Value) when is_pid(Value) -> erlang:list_to_binary(erlang:pid_to_list(Value));
any_to_binary(Value) when is_binary(Value) -> Value.
