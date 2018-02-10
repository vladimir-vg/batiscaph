-module(z__client_io_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(shell_io, {
  pending_get_until, % {From, ReplyAs, Prompt, Continuation, ExtraArgs}
  input_string = "", % just plain string that will be scanned
  receiver_pid
}).

-record(pending_read, {
  from,
  reply_as,
  prompt,
  continuation = "",
  extra_args,
  have_scanned = ""
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  State = #shell_io{},
  self() ! init,
  {ok, State}.



handle_info(init, #shell_io{receiver_pid = undefined} = State) ->
  {ok, ReceiverPid} = gen_server:call(z__client_scenario, get_remote_receiver_pid),
  {noreply, State#shell_io{receiver_pid = ReceiverPid}};

handle_info({input, Statement}, #shell_io{input_string = Input} = State) ->
  Input1 = Input ++ Statement, % just append and try to proceed input if pending
  {ok, State1} = continue_pending_input(State#shell_io{input_string = Input1}),
  {noreply, State1};

handle_info({io_request, From, ReplyAs, Request}, #shell_io{} = State) ->
  % State1 = refresh_stale_timer(State),
  {ok, State1} = handle_io_request(From, ReplyAs, Request, State),
  {noreply, State1};

handle_info(stale_alert, #shell_io{} = State) ->
  % Pid ! {self(), stalled},
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(pending_input, _From, State) ->
  {reply, pending_input(State), State};

handle_call(clear_pending, _From, #shell_io{pending_get_until = undefined} = State) ->
  {reply, ok, State};

handle_call(clear_pending, _From, #shell_io{pending_get_until = #pending_read{have_scanned = Scanned}, input_string = Input} = State) ->
  State1 = State#shell_io{pending_get_until = undefined, input_string = Scanned ++ Input},
  {reply, ok, State1};

handle_call(sync, _From, #shell_io{} = State) ->
  {reply, ok, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_io_request(From, ReplyAs, {put_chars,unicode,Binary}, #shell_io{} = State) when is_binary(Binary) ->
  Event = shell_output_event_now(From, [{put_chars,unicode,Binary}]),
  z__client_collector ! Event,
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(From, ReplyAs, {put_chars,unicode,io_lib,format,[Format,Args]}, #shell_io{} = State) ->
  Event = shell_output_event_now(From, [{put_chars,unicode,io_lib,format,[Format,Args]}]),
  z__client_collector ! Event,
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(From, ReplyAs, {get_until, unicode, Prompt, erl_scan, tokens, ExtraArgs}, #shell_io{pending_get_until = undefined, receiver_pid = ReceiverPid} = State) ->
  ReceiverPid ! {shell_input, {ready, Prompt}},
  Pending = #pending_read{from = From, reply_as = ReplyAs, prompt = Prompt, extra_args = ExtraArgs},
  {ok, State1} = continue_pending_input(State#shell_io{pending_get_until = Pending}),
  {ok, State1};

handle_io_request(From, ReplyAs, {get_geometry, _}, State) ->
  From ! {io_reply, ReplyAs, {error, request}},
  {ok, State};

handle_io_request(From, ReplyAs, getopts, State) ->
  From ! {io_reply, ReplyAs, [{binary,true},{encoding,unicode}]},
  {ok, State};

handle_io_request(From, ReplyAs, {requests, Requests}, #shell_io{} = State) ->
  Event = shell_output_event_now(From, Requests),
  z__client_collector ! Event,
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(_From, _ReplyAs, Request, State) ->
  io:format("unknown_request: ~p~n", [Request]),
  {ok, State}.



continue_pending_input(#shell_io{pending_get_until = undefined} = State) -> {ok, State};
continue_pending_input(#shell_io{pending_get_until = Pending, receiver_pid = ReceiverPid} = State) ->
  #pending_read{from = From, reply_as = ReplyAs, prompt = Prompt} = Pending,
  case attempt_scan(State#shell_io{pending_get_until = Pending}) of
    {ok, Scanned, Result, State1} ->
      Event = shell_input_event_now(From, Prompt, Scanned),

      % make sure that input logged first, and only then execution starts
      ok = gen_server:call(z__client_collector, {event, Event}),
      ReceiverPid ! {shell_input, stopped},
      From ! {io_reply, ReplyAs, Result},
      {ok, State1#shell_io{pending_get_until = undefined}};
    {need_more_input, State1} ->
      {ok, State1}
  end.



attempt_scan(#shell_io{input_string = Input, pending_get_until = Pending} = State) ->
  #pending_read{extra_args = ExtraArgs, continuation = Continuation, have_scanned = Scanned} = Pending,
  case erlang:apply(erl_scan, tokens, [Continuation, Input] ++ ExtraArgs) of
    {done, Result, Input1} ->
      Scanned1 = take_prefix(Input, Input1),
      {ok, Scanned ++ Scanned1, Result, State#shell_io{input_string = Input1}};
    {more, Continuation1} ->
      Pending1 = Pending#pending_read{continuation = Continuation1, have_scanned = Scanned ++ Input},
      {need_more_input, State#shell_io{input_string = "", pending_get_until = Pending1}}
  end.



take_prefix(Input, Input) -> "";
take_prefix([C | Input], Input) -> [C];
take_prefix([C | Input1], Input) -> [C | take_prefix(Input1, Input)].



shell_output_event_now(Pid, Requests) ->
  Now = erlang:system_time(micro_seconds),
  Output = shell_output_event_now0(Requests, []),
  #{
    <<"at_s">> => (Now div (1000*1000)),
    <<"at_mcs">> => (Now rem (1000*1000)),
    <<"pid">> => list_to_binary(pid_to_list(Pid)),
    <<"type">> => <<"shell_output">>,
    <<"message">> => iolist_to_binary(Output)
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
  Now = erlang:system_time(micro_seconds),
  #{
    <<"at_s">> => (Now div (1000*1000)),
    <<"at_mcs">> => (Now rem (1000*1000)),
    <<"pid">> => list_to_binary(pid_to_list(Pid)),
    <<"type">> => <<"shell_input">>,
    <<"prompt">> => iolist_to_binary(Prompt),
    <<"message">> => iolist_to_binary(Result)
  }.



pending_input(#shell_io{pending_get_until = undefined}) -> not_pending;
pending_input(#shell_io{pending_get_until = #pending_read{}, input_string = Input}) -> {pending, Input}.
