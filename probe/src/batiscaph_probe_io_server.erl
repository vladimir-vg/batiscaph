-module(batiscaph_probe_io_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-record(pending_read, {
  from,
  reply_as,
  prompt,
  continuation = "",
  extra_args,
  have_scanned = ""
}).

-record(io_server, {
  pending_get_until :: #pending_read{},
  input_string = "" % just plain string that will be scanned
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #io_server{}}.



handle_info({input, Statement}, #io_server{} = State) when is_binary(Statement) ->
  handle_info({input, binary_to_list(Statement)}, State);

handle_info({input, Statement}, #io_server{input_string = Input} = State) when is_list(Statement) ->
  Input1 = Input ++ Statement, % just append and try to proceed input if pending
  {ok, State1} = continue_pending_input(State#io_server{input_string = Input1}),
  {noreply, State1};

handle_info({io_request, From, ReplyAs, Request}, #io_server{} = State) ->
  {ok, State1} = handle_io_request(From, ReplyAs, Request, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_io_request(From, ReplyAs, {put_chars,unicode,Binary}, #io_server{} = State) when is_binary(Binary) ->
  Event = shell_output_event_now(From, [{put_chars,unicode,Binary}]),
  ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, [Event]}}),
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(From, ReplyAs, {put_chars,unicode,io_lib,format,[Format,Args]}, #io_server{} = State) ->
  Event = shell_output_event_now(From, [{put_chars,unicode,io_lib,format,[Format,Args]}]),
  ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, [Event]}}),
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(From, ReplyAs, {get_until, unicode, Prompt, erl_scan, tokens, ExtraArgs}, #io_server{pending_get_until = undefined} = State) ->
  batiscaph_probe_session ! {to_service, {shell_input, {ready, Prompt}}},
  Pending = #pending_read{from = From, reply_as = ReplyAs, prompt = Prompt, extra_args = ExtraArgs},
  {ok, State1} = continue_pending_input(State#io_server{pending_get_until = Pending}),
  {ok, State1};

handle_io_request(From, ReplyAs, {get_geometry, _}, State) ->
  From ! {io_reply, ReplyAs, {error, request}},
  {ok, State};

handle_io_request(From, ReplyAs, getopts, State) ->
  From ! {io_reply, ReplyAs, [{binary,true},{encoding,unicode}]},
  {ok, State};

handle_io_request(From, ReplyAs, {requests, Requests}, #io_server{} = State) ->
  Event = shell_output_event_now(From, Requests),
  ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, [Event]}}),
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(_From, _ReplyAs, Request, State) ->
  io:format("unknown io request: ~p~n", [Request]),
  {ok, State}.



continue_pending_input(#io_server{pending_get_until = undefined} = State) -> {ok, State};
continue_pending_input(#io_server{pending_get_until = Pending} = State) ->
  #pending_read{from = From, reply_as = ReplyAs, prompt = Prompt} = Pending,
  case attempt_scan(State#io_server{pending_get_until = Pending}) of
    {ok, Scanned, Result, State1} ->
      Event = shell_input_event_now(From, Prompt, Scanned),
      % make sure that input logged first, and only then execution starts
      batiscaph_probe_session ! {to_service, {shell_input, stopped}},
      ok = gen_server:call(batiscaph_probe_session, {queue_for_send, {events, [Event]}}),
      From ! {io_reply, ReplyAs, Result},
      {ok, State1#io_server{pending_get_until = undefined}};

    {need_more_input, State1} ->
      {ok, State1}
  end.



attempt_scan(#io_server{input_string = Input, pending_get_until = Pending} = State) ->
  % io:format("attempt_scan ~p~n", [Input]),
  #pending_read{extra_args = ExtraArgs, continuation = Continuation, have_scanned = Scanned} = Pending,
  case erlang:apply(erl_scan, tokens, [Continuation, Input] ++ ExtraArgs) of
    {done, Result, Input1} ->
      Scanned1 = take_prefix(Input, Input1),
      {ok, Scanned ++ Scanned1, Result, State#io_server{input_string = Input1}};

    {more, Continuation1} ->
      Pending1 = Pending#pending_read{continuation = Continuation1, have_scanned = Scanned ++ Input},
      {need_more_input, State#io_server{input_string = "", pending_get_until = Pending1}}
  end.



take_prefix(Input, Input) -> "";
take_prefix([C | Input], Input) -> [C];
take_prefix([C | Input1], Input) -> [C | take_prefix(Input1, Input)].



shell_output_event_now(Pid, Requests) ->
  Output = shell_output_event_now0(Requests, []),
  #{
    type => <<"p1 erlang:shell output">>,
    pid1 => list_to_binary(pid_to_list(Pid)),
    at => erlang:now(),
    output => iolist_to_binary(Output)
  }.

shell_output_event_now0([], Acc) -> lists:reverse(Acc);

shell_output_event_now0([{put_chars,unicode,Output} | Requests], Acc) ->
  shell_output_event_now0(Requests, [Output | Acc]);

shell_output_event_now0([{put_chars,latin1,Output} | Requests], Acc) ->
  shell_output_event_now0(Requests, [Output | Acc]);

shell_output_event_now0([{put_chars,unicode,io_lib,format,[Format,Args]} | Requests], Acc) ->
  Output = io_lib:format(Format, Args),
  shell_output_event_now0(Requests, [Output | Acc]).



shell_input_event_now(Pid, Prompt, Result) ->
  #{
    at => erlang:now(),
    type => <<"p1 erlang:shell input">>,
    pid1 => list_to_binary(pid_to_list(Pid)),
    prompt => iolist_to_binary(Prompt),
    input => iolist_to_binary(Result)
  }.
