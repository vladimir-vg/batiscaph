-module(es_collector).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(EVENTS_FLUSH_INTERVAL, 300).

-record(collector, {
  parent_pid,
  fd,
  ignored_pids = [],

  events_flush_timer,
  acc_events = []
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(ParentPid, Path) ->
  gen_server:start_link(?MODULE, [ParentPid, Path], []).

init([ParentPid, Path]) ->
  {ok, Fd} = file:open(Path, [write]),
  file:write(Fd, <<"at,at_mcs,type,pid,pid_arg,mfa,atom,prompt,message,term,size,hash\n">>),
  {ok, #collector{fd = Fd, parent_pid = ParentPid}}.



handle_info(flush_acc_events, #collector{events_flush_timer = Timer, acc_events = Events, parent_pid = ParentPid} = State) ->
  erlang:cancel_timer(Timer),
  ParentPid ! {events, lists:reverse(Events)},
  {noreply, State#collector{events_flush_timer = undefined, acc_events = []}};

handle_info(#{at := _, at_mcs := _, type := _} = Event, #collector{fd = Fd} = State) ->
  {ok, State1} = save_events_for_sending([Event], State),
  Output = format_event(Event),
  file:write(Fd, [Output, <<"\n">>]),
  {noreply, State1};

handle_info(Message, #collector{fd = Fd} = State) when element(1, Message) == trace_ts ->
  {ok, Events} = handle_trace_message(Message, State),
  {ok, State1} = save_events_for_sending(Events, State),
  [file:write(Fd, [format_event(E), <<"\n">>]) || E <- Events],
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



% blocking variant of event consumption
handle_call({event, E}, _From, State) ->
  {noreply, State1} = handle_info(E, State),
  {reply, ok, State1};

handle_call({ignore_pids_tracing, Pids1}, _From, #collector{ignored_pids = Pids} = State) ->
  Pids2 = [P || P <- Pids1 ++ Pids, erlang:is_process_alive(P)],
  {reply, ok, State#collector{ignored_pids = Pids2}};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



save_events_for_sending(Events, #collector{events_flush_timer = undefined} = State) ->
  Timer = erlang:send_after(?EVENTS_FLUSH_INTERVAL, self(), flush_acc_events),
  save_events_for_sending(Events, State#collector{events_flush_timer = Timer});

save_events_for_sending(Events1, #collector{acc_events = Events} = State) ->
  {ok, State#collector{acc_events = Events1 ++ Events}}.



format_event(#{at := At, at_mcs := Mcs, type := Type} = E) ->
  Pid = escape_string(iolist_to_binary(maps:get(pid, E, <<>>))),
  PidArg = escape_string(iolist_to_binary(maps:get(pid_arg, E, <<>>))),
  MFA = escape_string(iolist_to_binary(maps:get(mfa, E, <<>>))),
  Atom = escape_string(iolist_to_binary(maps:get(atom, E, <<>>))),
  Prompt = escape_string(iolist_to_binary(maps:get(prompt, E, <<>>))),
  Message = escape_string(iolist_to_binary(maps:get(message, E, <<>>))),
  Term = escape_string(iolist_to_binary(maps:get(term, E, <<>>))),
  Size = escape_string(integer_to_binary(maps:get(size, E, -1))),
  Hash = escape_string(iolist_to_binary(maps:get(hash, E, <<>>))),
  iolist_to_binary([
    integer_to_binary(At), ",", integer_to_binary(Mcs), ",", Type, ",",
    Pid, ",", PidArg, ",", MFA, ",", Atom, ",", Prompt, ",", Message, ",", Term, ",", Size, ",", Hash
  ]).

escape_string(<<>>) -> <<>>;
escape_string(Binary) -> <<"\"", (escape_string(Binary, <<>>))/binary, "\"">>.

escape_string(<<>>, Acc) -> Acc;
escape_string(<<"\"", Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, "\"\"">>);
escape_string(<<C, Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, C>>).



handle_trace_message({trace_ts, _Pid, send, _Msg, _PidTo, _} = Message, #collector{ignored_pids = _IgnoredPids}) ->
  % case (lists:member(Pid, IgnoredPids) orelse lists:member(PidTo, IgnoredPids)) of
  %   true -> {ok, []};
  %   false ->
  % end;
  handle_trace_message0(Message);

handle_trace_message(Message, #collector{ignored_pids = IgnoredPids}) ->
  Pid = element(2, Message),
  case lists:member(Pid, IgnoredPids) of
    true -> {ok, []};
    false ->
      handle_trace_message0(Message)
  end.

handle_trace_message0({trace_ts, _Pid, getting_linked, Port, _Timestmap}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, getting_unlinked, Port, _Timestmap}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, link, _PidPort, _Timestamp}) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, unlink, _PidPort, _Timestamp}) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, spawn, _ParentPid, _MFA, _Timestamp}) -> {ok, []};

handle_trace_message0({trace_ts, Pid, getting_linked, Pid1, Timestamp}) when is_pid(Pid1) ->
  E = #{type => <<"link">>, pid => erlang:pid_to_list(Pid), pid_arg => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, getting_unlinked, Pid1, Timestamp}) when is_pid(Pid1) ->
  E = #{type => <<"unlink">>, pid => erlang:pid_to_list(Pid), pid_arg => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, ChildPid, spawned, ParentPid, MFA, Timestamp}) ->
  MFA1 = mfa_str(MFA),
  E = #{type => <<"spawn">>, pid => erlang:pid_to_list(ChildPid), pid_arg => erlang:pid_to_list(ParentPid), mfa => MFA1},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, exit, Reason, Timestamp}) ->
  E = #{type => <<"exit">>, pid => erlang:pid_to_list(Pid), term => io_lib:format("~p", [Reason])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, register, Atom, Timestamp}) ->
  E = #{type => <<"register">>, pid => erlang:pid_to_list(Pid), atom => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, unregister, Atom, Timestamp}) ->
  E = #{type => <<"unregister">>, pid => erlang:pid_to_list(Pid), atom => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};


handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}) ->
  E = case To of
    _ when is_pid(To) -> #{pid_arg => erlang:pid_to_list(To)};
    _ when is_atom(To) -> #{atom => atom_to_binary(To, latin1)}
  end,
  E1 = E#{type => <<"send">>, pid => erlang:pid_to_list(Pid), term => io_lib:format("~p", [Msg])},
  E2 = event_with_timestamp(Timestamp, E1),
  {ok, [E2]};

handle_trace_message0({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}) ->
  To1 = case To of
    _ when is_pid(To) -> erlang:pid_to_list(To);
    _ when is_atom(To) -> atom_to_binary(To, latin1)
  end,
  E = #{type => <<"send_to_dead">>, pid => erlang:pid_to_list(Pid), pid_arg => To1, term => io_lib:format("~p", [Msg])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0(Message) ->
  io:format("skip trace message:~n~p~n", [Message]),
  {ok, []}.



event_with_timestamp({MegaSec, Sec, MicroSec}, E) ->
  Sec1 = MegaSec*1000*1000 + Sec,
  E1 = E#{at => Sec1, at_mcs => MicroSec},
  maps:fold(fun
    (pid, V, Acc) when is_list(V) -> Acc#{pid => list_to_binary(V)};
    (pid_arg, V, Acc) when is_list(V) -> Acc#{pid_arg => list_to_binary(V)};
    (term, V, Acc) when is_list(V) -> Acc#{term => list_to_binary(V)};
    (K, V, Acc) -> Acc#{K => V}
  end, #{}, E1).



mfa_str({proc_lib,init_p,[_Parent, _Ancestors, _Fun]}) -> <<"proc_lib:init_p/3">>;
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, gen, init_it, Args]}) -> mfa_str({gen, init_it, Args});
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, M, F, A]}) -> mfa_str({M, F, A});
mfa_str({gen,init_it,[gen_server, _, _, M, _A, _Opts]}) -> mfa_str({M,init,[placeholder]});
mfa_str({gen,init_it,[gen_server, _, _, _Name, M, _A, _Opts]}) -> mfa_str({M,init,[placeholder]});
mfa_str({M,F,A}) -> iolist_to_binary([atom_to_binary(M,latin1), ":", atom_to_binary(F,latin1), "/", integer_to_binary(length(A))]).