-module(scenarios_csv_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  {<<"/scenarios/", Path/binary>>, Req1} = cowboy_req:path(Req),
  {ok, Events} = clk_events:select(#{instance_id => Path}),
  {ok, CsvBody} = events_into_csv(Events),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/csv">>}], CsvBody, Req1),
  {ok, Req2, State}.



events_into_csv(Events) ->
  {ColNames, _Types} = lists:unzip(clk_events:columns()),
  ColNames1 = lists:delete(<<"instance_id">>, ColNames),
  <<",", ColNamesBin/binary>> = iolist_to_binary([[",", N] || N <- ColNames1]),
  events_into_csv(Events, ColNames1, <<ColNamesBin/binary, "\n">>).

events_into_csv([], _ColNames, Acc) -> {ok, Acc};
events_into_csv([E | Events], ColNames, Acc) ->
  <<",", Row/binary>> = lists:foldl(fun (Col, RowAcc) ->
    Val = to_string(maps:get(Col, E)),
    <<RowAcc/binary, ",", Val/binary>>
  end, <<>>, ColNames),
  events_into_csv(Events, ColNames, <<Acc/binary, Row/binary, "\n">>).



to_string(Binary) when is_binary(Binary) -> escape_string(Binary);
to_string(Int) when is_integer(Int) -> integer_to_binary(Int).



escape_string(<<>>) -> <<>>;
escape_string(Binary) -> <<"\"", (escape_string(Binary, <<>>))/binary, "\"">>.

escape_string(<<>>, Acc) -> Acc;
escape_string(<<"\"", Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, "\"\"">>);
escape_string(<<C, Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, C>>).
