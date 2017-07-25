-module(z__remote_collector).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(EVENTS_FLUSH_INTERVAL, 300).

-record(collector, {
  receiver_pid,
  ignored_pids = [],

  events_flush_timer,
  acc_events = []
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #collector{}}.



handle_info(flush_acc_events, #collector{} = State) ->
  {ok, State1} = flush_acc_events(State),
  {noreply, State1};

handle_info(#{<<"at">> := _, <<"at_mcs">> := _, <<"type">> := _} = Event, #collector{} = State) ->
  {ok, State1} = save_events_for_sending([Event], State),
  {noreply, State1};

handle_info(Message, #collector{} = State) when element(1, Message) == trace_ts ->
  {ok, Events} = handle_trace_message(Message, State),
  {ok, State1} = save_events_for_sending(Events, State),
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
  E = #{<<"type">> => <<"link">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, getting_unlinked, Pid1, Timestamp}) when is_pid(Pid1) ->
  E = #{<<"type">> => <<"unlink">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, ChildPid, spawned, ParentPid, MFA, Timestamp}) ->
  MFA1 = mfa_str(MFA),
  E = #{<<"type">> => <<"spawn">>, <<"pid">> => erlang:pid_to_list(ChildPid), <<"pid1">> => erlang:pid_to_list(ParentPid), <<"mfa">> => MFA1},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, exit, Reason, Timestamp}) ->
  E = #{<<"type">> => <<"exit">>, <<"pid">> => erlang:pid_to_list(Pid), <<"term">> => io_lib:format("~p", [Reason])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, register, Atom, Timestamp}) ->
  E = #{<<"type">> => <<"register">>, <<"pid">> => erlang:pid_to_list(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, unregister, Atom, Timestamp}) ->
  E = #{<<"type">> => <<"unregister">>, <<"pid">> => erlang:pid_to_list(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};


handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}) ->
  E = case To of
    _ when is_pid(To) -> #{<<"pid1">> => erlang:pid_to_list(To)};
    _ when is_atom(To) -> #{<<"atom">> => atom_to_binary(To, latin1)}
  end,
  E1 = E#{<<"type">> => <<"send">>, <<"pid">> => erlang:pid_to_list(Pid), <<"term">> => io_lib:format("~p", [Msg])},
  E2 = event_with_timestamp(Timestamp, E1),
  {ok, [E2]};

handle_trace_message0({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}) ->
  To1 = case To of
    _ when is_pid(To) -> erlang:pid_to_list(To);
    _ when is_atom(To) -> atom_to_binary(To, latin1)
  end,
  E = #{<<"type">> => <<"send_to_dead">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => To1, <<"term">> => io_lib:format("~p", [Msg])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0(Message) ->
  io:format("skip trace message:~n~p~n", [Message]),
  {ok, []}.



event_with_timestamp({MegaSec, Sec, MicroSec}, E) ->
  Sec1 = MegaSec*1000*1000 + Sec,
  E1 = E#{<<"at">> => Sec1, <<"at_mcs">> => MicroSec},
  maps:fold(fun
    (<<"pid">>, V, Acc) when is_list(V) -> Acc#{<<"pid">> => list_to_binary(V)};
    (<<"pid1">>, V, Acc) when is_list(V) -> Acc#{<<"pid1">> => list_to_binary(V)};
    (<<"term">>, V, Acc) when is_list(V) -> Acc#{<<"term">> => list_to_binary(V)};
    (K, V, Acc) -> Acc#{K => V}
  end, #{}, E1).



mfa_str({proc_lib,init_p,[_Parent, _Ancestors, _Fun]}) -> <<"proc_lib:init_p/3">>;
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, gen, init_it, Args]}) -> mfa_str({gen, init_it, Args});
mfa_str({proc_lib,init_p,[_Parent, _Ancestors, M, F, A]}) -> mfa_str({M, F, A});
mfa_str({gen,init_it,[gen_server, _, _, M, _A, _Opts]}) -> mfa_str({M,init,[placeholder]});
mfa_str({gen,init_it,[gen_server, _, _, _Name, M, _A, _Opts]}) -> mfa_str({M,init,[placeholder]});
mfa_str({M,F,A}) -> iolist_to_binary([atom_to_binary(M,latin1), ":", atom_to_binary(F,latin1), "/", integer_to_binary(length(A))]).



flush_acc_events(#collector{receiver_pid = undefined} = State) ->
  {ok, ReceiverPid} = gen_server:call(z__remote_scenario, get_remote_receiver_pid),
  flush_acc_events(State#collector{receiver_pid = ReceiverPid});

flush_acc_events(#collector{events_flush_timer = Timer, acc_events = Events, receiver_pid = ReceiverPid} = State) ->
  erlang:cancel_timer(Timer),
  ReceiverPid ! {events, lists:reverse(Events)},
  {ok, State#collector{events_flush_timer = undefined, acc_events = []}}.
