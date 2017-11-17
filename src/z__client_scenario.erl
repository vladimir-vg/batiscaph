-module(z__client_scenario).
-behaviour(gen_server).
-export([trace_started_events/2, trace_pid/1, trace_pid/2, clear_tracing/1]).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ESPACE_NODE_TIMEOUT, 5000).



%%% This module does node-to-node communication



-record(scenario, {
  opts,
  batiscaph_node,
  batiscaph_node_timer,
  receiver_pid,

  % supervisor monitor ref
  sup_mref
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(BatiscaphNode, ReceiverPid, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [BatiscaphNode, ReceiverPid, Opts], []).

init([BatiscaphNode, ReceiverPid, Opts]) ->
  self() ! setup,
  {ok, #scenario{batiscaph_node = BatiscaphNode, receiver_pid = ReceiverPid, opts = Opts}}.



handle_info(setup, State) ->
  {ok, State1} = setup(State),
  {noreply, State1};

handle_info(check_batiscaph_node, #scenario{batiscaph_node = BatiscaphNode} = State) ->
  ok = check_batiscaph_node(BatiscaphNode),
  {ok, State1} = refresh_batiscaph_node_timer(State),
  {noreply, State1};

handle_info({'DOWN', MRef, process, _Pid, Reason}, #scenario{sup_mref = MRef} = State) ->
  % stop node if we were monitoring supervisor and it died
  io:format("Scenario supervisor died, shutdown node: ~p\n", [Reason]),
  timer:sleep(1000),
  init:stop(),
  {noreply, State};

handle_info(shell_restart, State) ->
  ok = gen_server:call(z__client_shell, restart_shell),
  {noreply, State};

% handle_info({store_module, Name, Body}, State) ->
%   {ok, State1} = store_module(Name, Body, State),
%   {noreply, State1};

handle_info({shell_input, Input}, State) ->
  z__client_io_server ! {input, binary_to_list(Input)},
  {noreply, State};

handle_info({trace_pid, Pid}, State) when is_binary(Pid) ->
  try list_to_pid(binary_to_list(Pid)) of
    Pid1 ->
      ok = trace_pid(Pid1),
      {noreply, State}
  catch
    badarg ->
      {noreply, State}
  end;


handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(get_remote_receiver_pid, _From, #scenario{receiver_pid = ReceiverPid} = State) ->
  {reply, {ok, ReceiverPid}, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



refresh_batiscaph_node_timer(#scenario{batiscaph_node_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  refresh_batiscaph_node_timer(State#scenario{batiscaph_node_timer = undefined});

refresh_batiscaph_node_timer(#scenario{batiscaph_node_timer = undefined} = State) ->
  Timer = erlang:send_after(?ESPACE_NODE_TIMEOUT, self(), check_batiscaph_node),
  {ok, State#scenario{batiscaph_node_timer = Timer}}.



check_batiscaph_node(BatiscaphNode) ->
  case net_adm:ping(BatiscaphNode) of
    pong -> ok;
    pang ->
      % remote batiscaph is unavailable, shut down node
      io:format("Espace node is gone, shutting down\n"),
      timer:sleep(1000),
      init:stop()
  end.



setup(#scenario{opts = Opts} = State) ->
  {ok, SupPid} = z__client_sup:start_link(),

  {ok, State1} = case maps:get(nodestop_on_scenario_shutdown, Opts, undefined) of
    undefined -> {ok, State};
    true ->
      % if supervisor dies need to stop node
      unlink(SupPid),
      MRef = monitor(process, SupPid),
      {ok, State#scenario{sup_mref = MRef}}
  end,

  case maps:get(nodestop_on_disconnect, Opts, undefined) of
    undefined -> {ok, State1};
    true ->
      {ok, State2} = refresh_batiscaph_node_timer(State1),
      {ok, State2}
  end.



% store_module(Name, Body, #scenario{} = State) ->
%   Event = module_stored_event_now(Name, Body),
%   Event1 = case file:write_file(<<Name/binary, ".erl">>, Body) of
%     ok -> Event;
%     {error, Reason} ->
%       Event#{type => <<"module_storage_failed">>, term => iolist_to_binary(io_lib:format("~p", [Reason]))}
%   end,
%   z__client_collector ! Event1,
%   {ok, State}.
% 
% module_stored_event_now(Name, Body) ->
%   E = event_now(),
%   E#{
%     <<"type">> => <<"module_stored">>,
%     <<"atom">> => Name,
%     <<"size">> => byte_size(Body),
%     <<"hash">> => bin_to_hex:bin_to_hex(crypto:hash(md5, Body))
%   }.

% event_now() ->
%   Now = erlang:system_time(micro_seconds),
%   #{
%     <<"at_s">> => (Now div (1000*1000)),
%     <<"at_mcs">> => (Now rem (1000*1000))
%   }.



% this function enabled tracing if not already enabled,
% generates mention events about links and ancestors
trace_pid(Pid) -> trace_pid(Pid, #{}).

trace_pid(Pid, Opts) ->
  Flags = trace_flags(Opts),
  CollectorPid = case whereis(z__client_collector) of
    undefined -> error(try_trace_while_collector_is_dead);
    Pid1 -> Pid1
  end,

  case erlang:trace_info(Pid, flags) of
    undefined -> % dead
      E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
        <<"type">> => <<"found_dead">>,
        <<"pid">> => pid_to_list(Pid)
      }),
      CollectorPid ! E,
      ok;

    {flags, [_|_]} -> ok; % already traced
    {flags, []} ->
      try erlang:trace(Pid, true, [{tracer, CollectorPid} | Flags]) of
        1 ->
          Events = trace_started_events(erlang:system_time(micro_seconds), Pid, [with_mentions]),
          % io:format("got events: ~p\n",[Events]),
          % E = trace_started_event(erlang:system_time(micro_seconds), Pid),
          CollectorPid ! {events, Events},
          ok
      catch
        error:badarg ->
          case {erlang:is_process_alive(Pid), erlang:is_process_alive(CollectorPid)} of
            {_, false} -> error(try_trace_while_collector_is_dead);
            {true, true} -> error(failed_to_trace_alive_process);
            {false, true} ->
              % okay, process is dead already, meaningless to trace it
              % just record event that it was dead at this timestamp already
              E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
                <<"type">> => <<"found_dead">>,
                <<"pid">> => pid_to_list(Pid)
              }),
              % E = event_now(),
              % E1 = E#{<<"type">> => <<"found_dead">>, <<"pid">> => list_to_binary(pid_to_list(Pid))},
              CollectorPid ! E,
              ok
          end
      end
  end.



% z__client_collector:setup_global_tracing is used to set up trace_pattern
% for send, receive, call tracing specified here
trace_flags(#{set_on_spawn := true}) ->
  [procs, timestamp, set_on_spawn, send, 'receive', call];
trace_flags(#{}) ->
  [procs, timestamp, send, 'receive', call].



clear_tracing(Pid) ->
  try erlang:trace(Pid, false, trace_flags(#{})) of
    1 ->
      E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
        <<"type">> => <<"trace_stopped">>,
        <<"pid">> => erlang:pid_to_list(Pid)
      }),
      z__client_collector ! E,
      ok
  catch error:badarg ->
    false = erlang:is_process_alive(Pid),
    E = z__client_collector:event_with_timestamp(erlang:system_time(micro_seconds), #{
      <<"type">> => <<"found_dead">>,
      <<"pid">> => erlang:pid_to_list(Pid)
    }),
    z__client_collector ! E,
    ok
  end.



trace_started_events(Timestamp, Pid) -> trace_started_events(Timestamp, Pid, []).

trace_started_events(Timestamp, Pid, Opts) ->
  App = case application:get_application(Pid) of
    {ok, Atom} -> atom_to_binary(Atom, latin1);
    undefined -> <<>>
  end,
  case process_info(Pid, [dictionary, registered_name, trap_exit, links]) of
    undefined ->
      [z__client_collector:event_with_timestamp(Timestamp, #{
        <<"type">> => <<"found_dead">>,
        <<"pid">> => erlang:pid_to_list(Pid)
      })];

    Props when is_list(Props) ->
      Ancestors = proplists:get_value('$ancestors', proplists:get_value(dictionary, Props, []), []),
      Links = proplists:get_value(links, Props, []),
      RegName = case proplists:get_value(registered_name, Props) of
        [] -> <<>>;
        Atom1 when is_atom(Atom1) -> atom_to_binary(Atom1, latin1)
      end,
      TrapExit = case proplists:get_value(trap_exit, Props) of
        true -> 1;
        false -> 0
      end,

      OtherEvents = [],
      % temporary comment out mentioning ancestors and links
      % currently it only pollutes space, don't make things easier
      % 
      % OtherEvents = case lists:member(with_mentions, Opts) of
      %   true -> mention_events(Timestamp, Pid, Ancestors ++ Links);
      %   false -> []
      % end,
      E = z__client_collector:event_with_timestamp(Timestamp, #{
        <<"application">> => App,
        <<"ancestors">> => processes_list_to_binary(Ancestors),
        <<"links">> => processes_list_to_binary(Links),
        <<"trap_exit">> => TrapExit,
        <<"atom">> => RegName,
        <<"type">> => <<"trace_started">>,
        <<"pid">> => erlang:pid_to_list(Pid)
      }),
      [E | OtherEvents]
  end.

processes_list_to_binary(Ancestors) ->
  AncestorsBin = lists:map(fun
    (Atom) when is_atom(Atom) -> atom_to_binary(Atom,latin1);
    (Pid) when is_pid(Pid) -> pid_to_list(Pid);
    (Port) when is_port(Port) -> port_to_list(Port)
  end, Ancestors),
  iolist_to_binary(lists:join(" ", AncestorsBin)).



mention_events(_Timestamp, _FirstPid, []) -> [];
mention_events(Timestamp, FirstPid, [Port | Pids]) when is_port(Port) ->
  mention_events(Timestamp, FirstPid, Pids);
mention_events(Timestamp, FirstPid, [Atom | Pids]) when is_atom(Atom) ->
  case whereis(Atom) of
    undefined -> mention_events(Timestamp, FirstPid, Pids);
    Pid when is_pid(Pid) ->
      % maybe would be better to show mention not towards particular process,
      % but to registered name
      mention_events(Timestamp, FirstPid, [Pid | Pids])
  end;

mention_events(Timestamp, FirstPid, [Pid | Pids]) when is_pid(Pid) ->
  case process_info(Pid, [trap_exit]) of
    undefined ->
      E = z__client_collector:event_with_timestamp(Timestamp, #{
        <<"type">> => <<"found_dead">>,
        <<"pid">> => erlang:pid_to_list(Pid),
        <<"pid1">> => erlang:pid_to_list(FirstPid)
      }),
      [E | mention_events(Timestamp, FirstPid, Pids)];

    Props ->
      TrapExit = case proplists:get_value(trap_exit, Props) of
        true -> 1;
        false -> 0
      end,
      E = z__client_collector:event_with_timestamp(Timestamp, #{
        <<"type">> => <<"mention">>,
        <<"pid">> => erlang:pid_to_list(Pid),
        <<"pid1">> => erlang:pid_to_list(FirstPid),
        <<"trap_exit">> => TrapExit
      }),
      [E | mention_events(Timestamp, FirstPid, Pids)]
  end.
