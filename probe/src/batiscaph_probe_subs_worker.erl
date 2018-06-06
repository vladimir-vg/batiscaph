-module(batiscaph_probe_subs_worker).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(PROCESS_INFO_LOG_INTERVAL, 3000).



% This worker handles various subscriptions tasks.
% Just to extract some code from session process,
% to keep it simple.



-record(subs, {
  process_info_pids = [],
  process_info_timer,
  prev_process_infos = #{}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #subs{}}.



handle_info(log_process_infos, State) ->
  {ok, State1} = log_process_infos(State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call({ensure_subscribed_to_process_info, Pid}, _From, State) ->
  {ok, State1} = ensure_subscribed_to_process_info(Pid, State),
  {reply, ok, State1};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%
%
%



ensure_subscribed_to_process_info(Pid, #subs{process_info_pids = Pids} = State) ->
  case erlang:is_process_alive(Pid) of
    false -> {ok, State};
    true ->
      Pids1 = lists:usort([Pid | Pids]),
      State1 = State#subs{process_info_pids = Pids1},
      % log process_info right after enabling it
      % just to make it more responsive for user
      self() ! log_process_infos,
      {ok, State1}
  end.



log_process_infos(#subs{process_info_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  log_process_infos(State#subs{process_info_timer = undefined});

log_process_infos(#subs{process_info_timer = undefined} = State) ->
  {ok, State1} = log_process_infos1(State),
  Timer = erlang:send_after(?PROCESS_INFO_LOG_INTERVAL, self(), log_process_infos),
  State2 = State1#subs{process_info_timer = Timer},
  {ok, State2}.



log_process_infos1(#subs{process_info_pids = []} = State) ->
  {ok, State#subs{prev_process_infos = #{}}};

log_process_infos1(#subs{process_info_pids = OldPids, prev_process_infos = OldInfos} = State) ->
  {ok, Events, Pids, Infos} = log_process_infos2(OldPids, OldInfos),
  ok = gen_server:call(batiscaph_probe_session, {events, Events}),
  State1 = State#subs{process_info_pids = Pids, prev_process_infos = Infos},
  {ok, State1}.



log_process_infos2(Pids, OldInfos) ->
  log_process_infos2(Pids, OldInfos, [], [], #{}).

log_process_infos2([], _OldInfos, NewPids, Events, NewInfos) ->
  {ok, Events, NewPids, NewInfos};

log_process_infos2([Pid | Pids], OldInfos, NewPids, Events, NewInfos) ->
  case erlang:process_info(Pid, process_info_flags()) of
    undefined -> log_process_infos2(Pids, OldInfos, NewPids, Events, NewInfos);
    Props ->
      Timestamp = erlang:now(),
      Props1 = attrs_to_map(Props),
      case maps:get(Pid, OldInfos, undefined) of
        undefined ->
          Events1 = process_info_events(Pid, Timestamp, Props1),
          NewInfos1 = maps:put(Pid, Props1, NewInfos),
          log_process_infos2(Pids, OldInfos, [Pid | NewPids], Events1 ++ Events, NewInfos1);

        Props1 ->
          % exactly same props as previous
          % keep old infos, but do not generate event
          NewInfos1 = maps:put(Pid, Props1, NewInfos),
          log_process_infos2(Pids, OldInfos, [Pid | NewPids], Events, NewInfos1);

        Props2 ->
          % collect changed values and log them
          ChangedProps = diff_props(Props2, Props1),
          Events1 = process_info_events(Pid, Timestamp, ChangedProps),
          NewInfos1 = maps:put(Pid, Props1, NewInfos),
          log_process_infos2(Pids, OldInfos, [Pid | NewPids], Events1 ++ Events, NewInfos1)
      end
  end.



attrs_to_map(Props) ->
  % maps:take/2 is not available in Erlang 17
  Props0 = maps:from_list(Props),
  Binaries = maps:get(binary, Props0),
  Props1 = maps:remove(binary, Props0),
  Binaries1 = maps:from_list(lists:map(fun ({Id, Size, RefCount}) ->
    Key = integer_to_binary(Id),
    Value = <<(integer_to_binary(Size))/binary, " ", (integer_to_binary(RefCount))/binary>>,
    {Key, Value}
  end, Binaries)),
  Props1#{binary => Binaries1}.



diff_props(OldProps, NewProps) when is_map(OldProps) andalso is_map(NewProps) ->
  maps:fold(fun
    (binary, OldBinaries, Acc) ->
      case maps:get(binary, Acc, undefined) of
        undefined -> Acc;
        % same value in old and new
        % remove this prop, no need to log it
        OldBinaries -> maps:remove(binary, Acc);

        NewBinaries ->
          DiffBinaries = diff_props(OldBinaries, NewBinaries),
          Acc#{binary => DiffBinaries}
      end;

    % only arbitrary keys are binary (say, ids of binary chunks)
    % these keys may disappear from props
    % need to specifically report in diff their disappearance
    % by sending empty string in diff.
    (K, V, Acc) when is_binary(K) ->
      case maps:get(K, Acc, undefined) of
        % key is present in old props, but gone in new
        % need to report that it's gone
        undefined -> maps:put(K, <<>>, Acc);
        % same value in old and new
        % remove this prop, no need to log it
        V -> maps:remove(K, Acc);
        _ -> Acc
      end;

    (K, V, Acc) when is_atom(K) ->
      case maps:get(K, Acc, undefined) of
        undefined -> Acc;
        % same value in old and new
        % remove this prop, no need to log it
        V -> maps:remove(K, Acc);
        _ -> Acc
      end
  end, NewProps, OldProps).



process_info_flags() ->
  [
    current_function, initial_call, message_queue_len,
    registered_name, links, monitors, dictionary,
    binary
  ].

process_info_events(Pid, Timestamp, Props) ->
  {BinaryEvents, Props1} = case maps:get(binary, Props, undefined) of
    undefined -> {[], Props};
    Binaries ->
      E = #{
        at => Timestamp,
        type => <<"p1 erlang:process process_info_binary">>,
        pid1 => erlang:list_to_binary(erlang:pid_to_list(Pid)),
        binaries => erlang:term_to_binary(Binaries)
      },
      {[E], maps:remove(binary, Props)}
  end,
  Props3 = maps:map(fun format_props/2, Props1),

  BinaryEvents ++ [Props3#{
    at => Timestamp,
    type => <<"p1 erlang:process process_info">>,
    pid1 => erlang:list_to_binary(erlang:pid_to_list(Pid))
  }].



format_props(current_function, {M, F, A}) ->
  iolist_to_binary([atom_to_binary(M,latin1), ":", atom_to_binary(F,latin1), "/", integer_to_binary(A)]);
format_props(initial_call, {M, F, A}) ->
  iolist_to_binary([atom_to_binary(M,latin1), ":", atom_to_binary(F,latin1), "/", integer_to_binary(A)]);
format_props(message_queue_len, N) ->
  integer_to_binary(N);
format_props(registered_name, []) -> <<>>;
format_props(registered_name, Atom) when is_atom(Atom) ->
  atom_to_binary(Atom,latin1);

format_props(links, List) ->
  List1 = lists:map(fun
    (P) when is_pid(P) -> erlang:pid_to_list(P);
    (P) when is_port(P) -> erlang:port_to_list(P)
  end, List),
  iolist_to_binary(lists_join(<<" ">>, List1));

format_props(monitors, List) ->
  List1 = lists:map(fun
    ({process,P}) when is_pid(P) -> erlang:pid_to_list(P);
    ({port,P}) when is_port(P) -> erlang:port_to_list(P);
    ({time_offset,clock_service}) -> "time_offset"
  end, List),
  iolist_to_binary(lists_join(<<" ">>, List1));

format_props(dictionary, Term) ->
  batiscaph_probe_util:format_term(Term).



% 17 and 18 Erlang don't have lists:join/2
lists_join(_Sep, []) -> [];
lists_join(Sep, List) ->
  [_ | List2] = lists:flatten([[Sep, E] || E <- List]),
  List2.
