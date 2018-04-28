-module(old_vision_probe_collector).
-behaviour(gen_server).
-export([event_with_timestamp/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(EVENTS_FLUSH_INTERVAL, 300).

-record(collector, {
  receiver_pid,
  ignored_pids = [],

  events_flush_timer,
  acc_events = [],

  % remember current shell pid
  % to properly recognize shell specific trace events
  shell_pid,
  shell_bindings :: #{atom() => any()},

  % temporary state that needed for producing
  % one event from several trace events
  % TODO: from time to time need to cleanup this map, remove old failed attempts to change port owner
  change_port_owner_map = #{} :: #{{message, pid(), port()} => pid(), {call, pid()} => {pid(), port()}}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ok = setup_global_tracing(),
  % self() ! init,
  {ok, #collector{}}.



% handle_info(init, #collector{} = State) ->
%   {noreply, State};

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
  {ok, Events, State1} = handle_trace_message(Message, State),
  {ok, State2} = save_events_for_sending(Events, State1),
  {noreply, State2};

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

handle_call(sync, _From, #collector{} = State) ->
  {reply, ok, State};

handle_call({set_shell_pid, Pid}, _From, #collector{} = State) ->
  ok = setup_tracing_for_shell(Pid),
  {reply, ok, State#collector{shell_pid = Pid}};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



setup_global_tracing() ->
  % it's not possible to apply trace_pattern to ports
  % have to take process to port send
  % and check successful response afterwards
  % for port ownership change

  PortConnectSpec = {['$1', {'_', {connect, '_'}}], [{is_port, '$1'}], []},
  PortConnectedSpec = {['_', '_', {'$1', connected}], [{is_port, '$1'}], []},
  PortConnectReturnTraceSpec = {['$1', '$2'], [{'andalso', {is_port, '$1'}, {is_pid, '$2'}}], [{return_trace}]},

  % do not trace messages from and to collector
  % because they would generate traces about trace messages
  CollectorPid = self(),
  AllPidsExceptCollector = {
    ['$1', '_'],
    [{'andalso',
        {is_pid, '$1'},
        {'andalso',
          {'=/=', '$1', CollectorPid},
          {'=/=', self, CollectorPid}
        }
     }],
    []
  },

  erlang:trace_pattern(send, [AllPidsExceptCollector, PortConnectSpec], []),
  erlang:trace_pattern('receive', [PortConnectedSpec], []),
  erlang:trace_pattern({erlang, port_connect, 2}, [PortConnectReturnTraceSpec], [global]),

  erlang:trace(ports, true, [ports, timestamp, {tracer, self()}]),
  ok.



setup_tracing_for_shell(Pid) ->
  erlang:trace_pattern({shell, server_loop, 7}, true, [local]),
  erlang:trace(Pid, true, [call]),
  ok.



save_events_for_sending(Events, #collector{events_flush_timer = undefined} = State) ->
  Timer = erlang:send_after(?EVENTS_FLUSH_INTERVAL, self(), flush_acc_events),
  save_events_for_sending(Events, State#collector{events_flush_timer = Timer});

save_events_for_sending(Events1, #collector{acc_events = Events} = State) ->
  {ok, State#collector{acc_events = Events1 ++ Events}}.






% handle_trace_message({trace_ts, _Pid, send, _Msg, _PidTo, _} = Message, #collector{ignored_pids = _IgnoredPids} = State) ->
%   % case (lists:member(Pid, IgnoredPids) orelse lists:member(PidTo, IgnoredPids)) of
%   %   true -> {ok, []};
%   %   false ->
%   % end;
%   handle_trace_message0(Message, State);

handle_trace_message(Message, #collector{ignored_pids = _IgnoredPids} = State) ->
  % Pid = element(2, Message),
  % case lists:member(Pid, IgnoredPids) of
  %   true -> {ok, []};
  %   false ->
  % end.
  handle_trace_message0(Message, State).


handle_trace_message0({trace_ts, Pid, call, {shell, server_loop, Args}, Timestamp}, #collector{shell_pid = Pid, shell_bindings = Map} = State) ->
  [_, _, Bindings, _, _, _, _] = Args,
  {ok, Pids, Events, Map1} = walk_bindings(Timestamp, Pid, Bindings, Map),
  % all mentioned processes are automatically traced
  [z__client_scenario:trace_pid(P) || P <- Pids],
  {ok, Events, State#collector{shell_bindings = Map1}};

handle_trace_message0({trace_ts, _Pid, getting_linked, Port, _Timestmap}, State) when is_port(Port) -> {ok, [], State};
handle_trace_message0({trace_ts, _Pid, getting_unlinked, Port, _Timestmap}, State) when is_port(Port) -> {ok, [], State};
handle_trace_message0({trace_ts, _Pid, link, _PidPort, _Timestamp}, State) -> {ok, [], State};
handle_trace_message0({trace_ts, _Pid, unlink, _PidPort, _Timestamp}, State) -> {ok, [], State};

handle_trace_message0({trace_ts, Pid, getting_linked, Pid1, Timestamp}, State) when is_pid(Pid) andalso is_pid(Pid1) ->
  E = #{<<"type">> => <<"link">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"pid1">> => z__client_scenario:format_term(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1], State};

handle_trace_message0({trace_ts, Port, getting_linked, Pid, _Timestamp}, State) when is_port(Port) andalso is_pid(Pid) ->
  % ignore for now
  {ok, [], State};
handle_trace_message0({trace_ts, Port, getting_unlinked, Pid, _Timestamp}, State) when is_port(Port) andalso is_pid(Pid) ->
  % ignore for now
  {ok, [], State};

handle_trace_message0({trace_ts, Pid, getting_unlinked, Pid1, Timestamp}, State) when is_pid(Pid) andalso is_pid(Pid1) ->
  E = #{<<"type">> => <<"unlink">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"pid1">> => z__client_scenario:format_term(Pid1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1], State};



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
handle_trace_message0({trace_ts, ParentPid, spawn, ChildPid, MFA, Timestamp}, State) ->
  case erlang:trace_info(ParentPid, flags) of
    undefined -> {ok, [], State};
    {flags, Flags} ->
      case lists:member(set_on_spawn, Flags) of
        true -> {ok, [], State}; % everything will be processed in spawned event
        false ->
          MFA1 = mfa_str(MFA),
          E = #{<<"type">> => <<"spawn">>, <<"pid">> => z__client_scenario:format_term(ChildPid), <<"pid1">> => z__client_scenario:format_term(ParentPid), <<"mfa">> => MFA1},
          E1 = event_with_timestamp(Timestamp, E),
          {ok, [E1], State}
      end
  end;

% spawned is received only if ChildPid is already traced (unlike spawn)
handle_trace_message0({trace_ts, ChildPid, spawned, ParentPid, MFA, Timestamp}, State) ->
  MFA1 = mfa_str(MFA),
  E = #{<<"type">> => <<"spawn">>, <<"pid">> => z__client_scenario:format_term(ChildPid), <<"pid1">> => z__client_scenario:format_term(ParentPid), <<"mfa">> => MFA1},
  E1 = event_with_timestamp(Timestamp, E),
  Events = z__client_scenario:trace_started_events(Timestamp, ChildPid),
  % F = #{<<"type">> => <<"trace_started">>, <<"pid">> => pid_to_list(ChildPid)},
  % F1 = event_with_timestamp(Timestamp, F),
  {ok, [E1 | Events], State};



handle_trace_message0({trace_ts, Pid, exit, Reason, Timestamp}, State) ->
  E = #{<<"type">> => <<"exit">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"term">> => z__client_scenario:format_term(Reason)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1], State};

handle_trace_message0({trace_ts, Pid, register, Atom, Timestamp}, State) when is_pid(Pid) ->
  E = #{<<"type">> => <<"register">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1], State};

handle_trace_message0({trace_ts, Pid, unregister, Atom, Timestamp}, State) when is_pid(Pid) ->
  E = #{<<"type">> => <<"unregister">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"atom">> => atom_to_binary(Atom,latin1)},
  E1 = event_with_timestamp(Timestamp, E),
  {ok, [E1], State};



handle_trace_message0({trace_ts, Port, open, Pid, DriverName, Timestamp}, State) when is_port(Port) ->
  E = event_with_timestamp(Timestamp, #{
    <<"type">> => <<"port_open">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"port">> => z__client_scenario:format_term(Port),
    <<"atom">> => atom_to_binary(DriverName,latin1)
  }),
  {ok, [E], State};

handle_trace_message0({trace_ts, Port, closed, Reason, Timestamp}, State) when is_port(Port) ->
  E = event_with_timestamp(Timestamp, #{
    <<"type">> => <<"port_close">>, <<"pid">> => <<>>, <<"port">> => z__client_scenario:format_term(Port),
    <<"term">> => z__client_scenario:format_term(Reason)
  }),
  {ok, [E], State};



% save message that indicates intent to change port owner
% we don't know yet is it going to succeed or not
% need to receive second message confirming change
handle_trace_message0({trace_ts, Pid, send, {Pid, {connect, NewOwner}}, Port, _Timestamp}, State)
when is_pid(Pid) andalso is_pid(NewOwner) andalso is_port(Port) ->
  #collector{change_port_owner_map = Map} = State,
  Map1 = maps:put({message, Pid, Port}, NewOwner, Map),
  State1 = State#collector{change_port_owner_map = Map1},
  {ok, [], State1};

handle_trace_message0({trace_ts, Pid, 'receive', {Port, connected}, Timestamp}, State)
when is_pid(Pid) andalso is_port(Port) ->
  #collector{change_port_owner_map = Map} = State,
  case maps:take({message, Pid, Port}, Map) of
    error ->
      io:format("received {~p, connected}, but seems like no {connect, ...} was made\n", [Port]),
      {ok, [], State};

    {NewOwner, Map1} when is_pid(NewOwner) ->
      E = event_with_timestamp(Timestamp, #{
        <<"type">> => <<"port_owner_change">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"port">> => z__client_scenario:format_term(Port),
        <<"pid1">> => z__client_scenario:format_term(NewOwner)
      }),
      State1 = State#collector{change_port_owner_map = Map1},
      {ok, [E], State1}
  end;



handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}, State)
when is_pid(Pid) andalso is_pid(To) ->
  E1 = #{<<"type">> => <<"send">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"pid1">> => z__client_scenario:format_term(To), <<"term">> => z__client_scenario:format_term(Msg)},
  E2 = event_with_timestamp(Timestamp, E1),
  {ok, [E2], State};

handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}, State)
when is_pid(Pid) andalso is_atom(To) ->
  % ignore for now
  {ok, [], State};

handle_trace_message0({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}, State)
when is_pid(Pid) andalso is_pid(To) ->
  E1 = #{<<"type">> => <<"send">>, <<"pid">> => z__client_scenario:format_term(Pid), pid1 => z__client_scenario:format_term(To), <<"term">> => z__client_scenario:format_term(Msg)},
  E2 = event_with_timestamp(Timestamp, E1),
  {ok, [E2], State};

handle_trace_message0({trace_ts, Port, send, Msg, To, Timestamp}, State) when is_port(Port) ->
  {ok, [], State};

handle_trace_message0({trace_ts, Port, send_to_non_existing_process, Msg, To, Timestamp}, State) when is_port(Port) ->
  {ok, [], State};

% handle_trace_message0({trace_ts, Pid, send, Msg, To, Timestamp}) when is_pid(Pid) ->
%   E = case To of
%     _ when is_pid(To) -> #{<<"pid1">> => pid_to_list(To)};
%     _ when is_atom(To) -> #{<<"atom">> => atom_to_binary(To, latin1)}
%   end,
%   E1 = E#{<<"type">> => <<"send">>, <<"pid">> => pid_to_list(Pid), <<"term">> => io_lib:format("~p", [Msg])},
%   E2 = event_with_timestamp(Timestamp, E1),
%   {ok, [E2]};

% handle_trace_message0({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}) when is_pid(Pid) ->
%   To1 = case To of
%     _ when is_pid(To) -> pid_to_list(To);
%     _ when is_atom(To) -> atom_to_binary(To, latin1)
%   end,
%   E = #{<<"type">> => <<"send_to_dead">>, <<"pid">> => pid_to_list(Pid), <<"pid1">> => To1, <<"term">> => io_lib:format("~p", [Msg])},
%   E1 = event_with_timestamp(Timestamp, E),
%   {ok, [E1]};



% trace port owner change by calling erlang:port_connect/2
handle_trace_message0({trace_ts, Pid, call, {erlang, port_connect, [Port, NewOwner]}, _Timestamp}, State) ->
  #collector{change_port_owner_map = Map} = State,
  Map1 = maps:put({call, Pid}, {NewOwner, Port}, Map),
  State1 = State#collector{change_port_owner_map = Map1},
  {ok, [], State1};

handle_trace_message0({trace_ts, Pid, return_from, {erlang, port_connect, 2}, true, Timestamp}, State) ->
  #collector{change_port_owner_map = Map} = State,
  case maps:take({call, Pid}, Map) of
    error ->
      io:format("got return_from erlang:port_connext/2 in ~p, but seems like no call was started\n", [Pid]),
      {ok, [], State};

    {{NewOwner, Port}, Map1} when is_pid(NewOwner) andalso is_port(Port) ->
      E = event_with_timestamp(Timestamp, #{
        <<"type">> => <<"port_owner_change">>, <<"pid">> => z__client_scenario:format_term(Pid), <<"port">> => z__client_scenario:format_term(Port),
        <<"pid1">> => z__client_scenario:format_term(NewOwner)
      }),
      State1 = State#collector{change_port_owner_map = Map1},
      {ok, [E], State1}
  end;



handle_trace_message0(Message, State) ->
  io:format("-----------------:~p~n", [Message]),
  {ok, [], State}.



walk_bindings(Timestamp, Pid, Bindings, Map) ->
  Acc = {#{}, sets:new(), []},
  walk_bindings(Timestamp, Pid, Bindings, Map, Acc).

walk_bindings(_Timestamp, _Pid, [], _OldMap, Acc) ->
  {NewMap, PidsSet, AccEvents} = Acc,
  {ok, sets:to_list(PidsSet), lists:reverse(AccEvents), NewMap};

walk_bindings(Timestamp, Pid, [{Key, Value} | Bindings], OldMap, Acc) ->
  {NewMap, PidsSet, AccEvents} = Acc,
  % Value might be anything, as well as 'to_avoid_exception' atom
  % to ensure that key actually present, also call maps:is_key/2
  case {maps:is_key(Key, OldMap), maps:get(Key, OldMap, to_avoid_exception)} of
    % nothing changed, same value in place
    {true, Value} ->
      Acc1 = {maps:put(Key, Value, NewMap), PidsSet, AccEvents},
      walk_bindings(Timestamp, Pid, Bindings, OldMap, Acc1);

    % value has changed for same key, should be reported
    {true, Value1} ->
      Events = maybe_var_mention_events(Timestamp, Pid, Key, Value1),
      PidsSet1 = collect_pids_from_term(Value1, PidsSet),
      Acc1 = {maps:put(Key, Value1, NewMap), PidsSet1, Events ++ AccEvents},
      walk_bindings(Timestamp, Pid, Bindings, OldMap, Acc1);

    % new binding
    {false, _} ->
      Events = maybe_var_mention_events(Timestamp, Pid, Key, Value),
      PidsSet1 = collect_pids_from_term(Value, PidsSet),
      Acc1 = {maps:put(Key, Value, NewMap), PidsSet1, Events ++ AccEvents},
      walk_bindings(Timestamp, Pid, Bindings, OldMap, Acc1)
  end.

maybe_var_mention_events(Timestamp, Pid, Var, Value) ->
  Context = z__client_shell:shell_context(Pid),
  lists:flatten(batiscaph_steps:var_mention_events(Timestamp, Pid, Var, Value, Context)).



collect_pids_from_term(Term, Set) when is_pid(Term) ->
  sets:add_element(Term, Set);

collect_pids_from_term(Term, Set) when is_list(Term) ->
  lists:foldl(fun collect_pids_from_term/2, Set, Term);

collect_pids_from_term(Term, Set) when is_tuple(Term) ->
  Size = erlang:tuple_size(Term),
  lists:foldl(fun (I, Set1) ->
    collect_pids_from_term(element(I, Term), Set1)
  end, Set, lists:seq(1, Size));

collect_pids_from_term(_Term, Set) ->
  Set.





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
    (K, V, Acc) -> maps:put(K, V, Acc)
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

