-module(etg_collector).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(collector, {
  fd
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Path) ->
  gen_server:start_link(?MODULE, [Path], []).

init([Path]) ->
  {ok, Fd} = file:open(Path, [write]),
  file:write(Fd, <<"at,at_mcs,type,prompt,message\n">>),
  {ok, #collector{fd=Fd}}.



handle_info(#{at := _, at_mcs := _, type := _} = Event, #collector{fd=Fd} = State) ->
  Output = format_event(Event),
  file:write(Fd, [Output, <<"\n">>]),
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

format_event(#{at := At, at_mcs := Mcs, type := Type} = E) ->
  Prompt = escape_string(iolist_to_binary(maps:get(prompt, E, <<>>))),
  Message = escape_string(iolist_to_binary(maps:get(message, E, <<>>))),
  iolist_to_binary([integer_to_binary(At), ",", integer_to_binary(Mcs), ",", Type, ",", Prompt, ",", Message]).

escape_string(<<>>) -> <<>>;
escape_string(Binary) -> <<"\"", (escape_string(Binary, <<>>))/binary, "\"">>.

escape_string(<<>>, Acc) -> Acc;
escape_string(<<"\"", Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, "\"\"">>);
escape_string(<<C, Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, C>>).
