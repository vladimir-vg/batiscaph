-module(z__client_collector).
-behaviour(gen_server).
-export([event_with_timestamp/2]).
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
  self() ! init,
  {ok, #collector{}}.



handle_info(init, #collector{} = State) ->
  ok = setup_global_tracing(),
  {noreply, State};

handle_info(flush_acc_events, #collector{} = State) ->
  {ok, State1} = flush_acc_events(State),
  {noreply, State1};

handle_info({events, Events}, #collector{} = State) ->
  {ok, State1} = save_events_for_sending(Events, State),
  {noreply, State1};

handle_info(#{<<"at_s">> := _, <<"at_mcs">> := _, <<"type">> := _} = Event, #collector{} = State) ->
  {ok, State1} = save_events_for_sending([Event], State),
  {noreply, State1};

handle_info(Message, #collector{} = State) when element(1, Message) == trace_ts ->
  {ok, Events} = handle_trace_message(Message, State),
  {ok, State1} = save_events_for_sending(Events, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(flush, _From, State) ->
  {noreply, State1} = handle_info(flush_acc_events, State),
  {reply, ok, State1};

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



setup_global_tracing() ->
  erlang:trace(ports, true, [ports, timestamp, {tracer, self()}]),
  % TODO: trace port owner change
  % call erlang:port_connect with return
  % receive of {Owner, {connect, Pid}}
  ok.



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

handle_trace_message(Message, #collector{ignored_pids = _IgnoredPids}) ->
  % Pid = element(2, Message),
  % case lists:member(Pid, IgnoredPids) of
  %   true -> {ok, []};
  %   false ->
  % end.
  handle_trace_message0(Message).

handle_trace_message0({trace_ts, _Pid, getting_linked, Port, _Timestmap}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, getting_unlinked, Port, _Timestmap}) when is_port(Port) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, link, _PidPort, _Timestamp}) -> {ok, []};
handle_trace_message0({trace_ts, _Pid, unlink, _PidPort, _Timestamp}) -> {ok, []};

handle_trace_message0({trace_ts, Pid, getting_linked, Pid1, Timestamp}) when is_pid(Pid) andalso is_pid(Pid1) ->
  E = #{<<"type">> => <<"link">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Port, getting_linked, Pid, _Timestamp}) when is_port(Port) andalso is_pid(Pid) ->
  % ignore for now
  {ok, []};

handle_trace_message0({trace_ts, Pid, getting_unlinked, Pid1, Timestamp}) when is_pid(Pid) andalso is_pid(Pid1) ->
  E = #{<<"type">> => <<"unlink">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => erlang:pid_to_list(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};



% We may receive both 'spawn' and 'spawned' events, if 'set_on_spawn' is set.
% But we should send only one 'spawn' event to collector.
% Also if it was 'spawned' event, then 'trace_started' event should be also generated for child.
% Better trace_started event to have exactly same timestamp as spawn event.
% But we don't know in advance when processing 'spawn' event would 'spawned' follow or not.
%
% To handle that we just check tracing flags on parent process, is there set_on_spawn or not.
% if it there, then do not generate events, they would be generated in 'spawned' clause.
%
% This is temporary solution. It will not work with set_on_first_spawn flag.
% It will also fail if tracing was cleared on parent process before collector consumed 'spawn' event.
handle_trace_message0({trace_ts, ParentPid, spawn, ChildPid, MFA, Timestamp}) ->
  case erlang:trace_info(ParentPid, flags) of
    undefined -> {ok, []};
    {flags, Flags} ->
      case lists:member(set_on_spawn, Flags) of
        true -> {ok, []}; % everything will be processed in spawned event
        false ->
          MFA1 = mfa_str(MFA),
          E = #{<<"type">> => <<"spawn">>, <<"pid">> => erlang:pid_to_list(ChildPid), <<"pid1">> => erlang:pid_to_list(ParentPid), <<"mfa">> => MFA1},
          E1 = event_with_timestamp(Timestamp, E),
          {ok, [E1]}
      end
  end;

% spawned is received only if ChildPid is already traced (unlike spawn)
handle_trace_message0({trace_ts, ChildPid, spawned, ParentPid, MFA, Timestamp}) ->
  MFA1 = mfa_str(MFA),
  E = #{<<"type">> => <<"spawn">>, <<"pid">> => erlang:pid_to_list(ChildPid), <<"pid1">> => erlang:pid_to_list(ParentPid), <<"mfa">> => MFA1},
  E1 = event_with_timestamp(Timestamp, E),
  [F] = z__client_scenario:trace_started_events(Timestamp, ChildPid),
  % F = #{<<"type">> => <<"trace_started">>, <<"pid">> => erlang:pid_to_list(ChildPid)},
  % F1 = event_with_timestamp(Timestamp, F),
  {ok, [E1, F]};



handle_trace_message0({trace_ts, Pid, exit, Reason, Timestamp}) ->
  E = #{<<"type">> => <<"exit">>, <<"pid">> => erlang:pid_to_list(Pid), <<"term">> => io_lib:format("~p", [Reason])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, register, Atom, Timestamp}) when is_pid(Pid) ->
  E = #{<<"type">> => <<"register">>, <<"pid">> => erlang:pid_to_list(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};

handle_trace_message0({trace_ts, Pid, unregister, Atom, Timestamp}) when is_pid(Pid) ->
  E = #{<<"type">> => <<"unregister">>, <<"pid">> => erlang:pid_to_list(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};



handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}) when is_pid(Pid) ->
  E = case To of
    _ when is_pid(To) -> #{<<"pid1">> => erlang:pid_to_list(To)};
    _ when is_atom(To) -> #{<<"atom">> => atom_to_binary(To, latin1)}
  end,
  E1 = E#{<<"type">> => <<"send">>, <<"pid">> => erlang:pid_to_list(Pid), <<"term">> => io_lib:format("~p", [Msg])},
  E2 = event_with_timestamp(Timestamp, E1),
  {ok, [E2]};

handle_trace_message0({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}) when is_pid(Pid) ->
  To1 = case To of
    _ when is_pid(To) -> erlang:pid_to_list(To);
    _ when is_atom(To) -> atom_to_binary(To, latin1)
  end,
  E = #{<<"type">> => <<"send_to_dead">>, <<"pid">> => erlang:pid_to_list(Pid), <<"pid1">> => To1, <<"term">> => io_lib:format("~p", [Msg])},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1]};



handle_trace_message0({trace_ts, Port, open, Pid, DriverName, Timestamp}) when is_port(Port) ->
  E = event_with_timestamp(Timestamp, #{
    <<"type">> => <<"port_open">>, <<"pid">> => io_lib:format("~p", [Pid]), <<"port">> => io_lib:format("~p", [Port]),
    <<"atom">> => atom_to_binary(DriverName,latin1)
  }),
  {ok, [E]};

handle_trace_message0({trace_ts, Port, closed, Reason, Timestamp}) when is_port(Port) ->
  E = event_with_timestamp(Timestamp, #{
    <<"type">> => <<"port_close">>, <<"pid">> => <<>>, <<"port">> => io_lib:format("~p", [Port]),
    <<"term">> => io_lib:format("~p", [Reason])
  }),
  {ok, [E]};



handle_trace_message0(Message) ->
  io:format("skip trace message:~n~p~n", [Message]),
  {ok, []}.






event_with_timestamp({MegaSec, Sec, MicroSec}, E) ->
  Sec1 = MegaSec*1000*1000 + Sec,
  event_with_timestamp({Sec1, MicroSec}, E);

event_with_timestamp(MicroSec, E) when is_integer(MicroSec) ->
  Sec1 = MicroSec div (1000*1000),
  MicroSec1 = MicroSec rem (1000*1000),
  event_with_timestamp({Sec1, MicroSec1}, E);

event_with_timestamp({Sec, MicroSec}, E) ->
  E1 = E#{<<"at_s">> => Sec, <<"at_mcs">> => MicroSec},
  maps:fold(fun
    (<<"pid">>, V, Acc) when is_list(V) -> Acc#{<<"pid">> => list_to_binary(V)};
    (<<"port">>, V, Acc) when is_list(V) -> Acc#{<<"port">> => list_to_binary(V)};
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
  {ok, ReceiverPid} = gen_server:call(z__client_scenario, get_remote_receiver_pid),
  flush_acc_events(State#collector{receiver_pid = ReceiverPid});

flush_acc_events(#collector{events_flush_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  flush_acc_events(State#collector{events_flush_timer = undefined});

flush_acc_events(#collector{events_flush_timer = undefined, acc_events = Events, receiver_pid = ReceiverPid} = State) ->
  ReceiverPid ! {events, lists:reverse(Events)},
  {ok, State#collector{acc_events = []}}.

