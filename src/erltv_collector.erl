-module(erltv_collector).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(collector, {
  fd,
  ignored_pids = []
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Path) ->
  gen_server:start_link(?MODULE, [Path], []).

init([Path]) ->
  {ok, Fd} = file:open(Path, [write]),
  file:write(Fd, <<"at,at_mcs,type,pid,pid_arg,mfa,atom,prompt,message,term\n">>),
  {ok, #collector{fd = Fd}}.



handle_info(#{at := _, at_mcs := _, type := _} = Event, #collector{fd = Fd} = State) ->
  Output = format_event(Event),
  file:write(Fd, [Output, <<"\n">>]),
  {noreply, State};

handle_info(Message, #collector{fd = Fd} = State) when element(1, Message) == trace_ts ->
  {ok, Events} = handle_trace_message(Message, State),
  [file:write(Fd, [format_event(E), <<"\n">>]) || E <- Events],
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



% blocking variant of event consumption
handle_call({event, E}, _From, State) ->
  {noreply, State1} = handle_info(E, State),
  {reply, ok, State1};

handle_call({ignore_pids_tracing, Pids}, _From, State) ->
  {reply, ok, State#collector{ignored_pids = Pids}};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



format_event(#{at := At, at_mcs := Mcs, type := Type} = E) ->
  Pid = escape_string(iolist_to_binary(maps:get(pid, E, <<>>))),
  PidArg = escape_string(iolist_to_binary(maps:get(pid_arg, E, <<>>))),
  MFA = escape_string(iolist_to_binary(maps:get(mfa, E, <<>>))),
  Atom = escape_string(iolist_to_binary(maps:get(atom, E, <<>>))),
  Prompt = escape_string(iolist_to_binary(maps:get(prompt, E, <<>>))),
  Message = escape_string(iolist_to_binary(maps:get(message, E, <<>>))),
  Term = escape_string(iolist_to_binary(maps:get(term, E, <<>>))),
  iolist_to_binary([
    integer_to_binary(At), ",", integer_to_binary(Mcs), ",", Type, ",",
    Pid, ",", PidArg, ",", MFA, ",", Atom, ",", Prompt, ",", Message, ",", Term
  ]).

escape_string(<<>>) -> <<>>;
escape_string(Binary) -> <<"\"", (escape_string(Binary, <<>>))/binary, "\"">>.

escape_string(<<>>, Acc) -> Acc;
escape_string(<<"\"", Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, "\"\"">>);
escape_string(<<C, Binary/binary>>, Acc) -> escape_string(Binary, <<Acc/binary, C>>).



handle_trace_message({trace_ts, Pid, send, _, PidTo, _} = Message, #collector{ignored_pids = IgnoredPids}) ->
  case (lists:member(Pid, IgnoredPids) orelse lists:member(PidTo, IgnoredPids)) of
    true -> {ok, []};
    false -> handle_trace_message0(Message)
  end;

handle_trace_message(Message, #collector{ignored_pids = IgnoredPids}) ->
  Pid = element(2, Message),
  case lists:member(Pid, IgnoredPids) of
    true -> {ok, []};
    false -> handle_trace_message0(Message)
  end.

handle_trace_message0({trace_ts, _Pid, getting_linked, _PidPort, _Timestmap}) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, getting_unlinked, _PidPort, _Timestmap}) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, link, Port, _Timestamp}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, unlink, Port, _Timestamp}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, spawned, _ChildPid, _MFA, _Timestamp}) -> {ok, []};

handle_trace_message0({trace_ts, Pid, link, Pid1, Timestamp}) when is_pid(Pid1) ->
  E = #{type => <<"link">>, pid => erlang:pid_to_list(Pid), pid_arg => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, unlink, Pid1, Timestamp}) when is_pid(Pid1) ->
  E = #{type => <<"unlink">>, pid => erlang:pid_to_list(Pid), pid_arg => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, ParentPid, spawn, ChildPid, MFA, Timestamp}) ->
  MFA1 = mfa_str(MFA),
  E = #{type => <<"spawn">>, pid => erlang:pid_to_list(ParentPid), pid_arg => erlang:pid_to_list(ChildPid), mfa => MFA1},
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
  E#{at => Sec1, at_mcs => MicroSec}.



mfa_str({proc_lib,init_p,[_Parent, _Ancestors, _Fun]}) -> <<"proc_lib:init_p/3">>;
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, gen, init_it, Args]}) -> mfa_str({gen, init_it, Args});
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, M, F, A]}) -> mfa_str({M, F, A});
mfa_str({gen,init_it,[gen_server, _, _, M, A, _Opts]}) -> mfa_str({M,init,A});
mfa_str({gen,init_it,[gen_server, _, _, _Name, M, A, _Opts]}) -> mfa_str({M,init,A});
mfa_str({M,F,A}) -> iolist_to_binary([atom_to_binary(M,latin1), ":", atom_to_binary(F,latin1), "/", integer_to_binary(length(A))]).