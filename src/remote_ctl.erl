-module(remote_ctl).
-behaviour(gen_server).
-export([ensure_started/1, ensure_started/2, currently_running/1, delta_json/1]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAX_EVENTS_PER_FETCH, 500).



%%% This process accept events from remote node
%%% and stores it to database. Sends prepared events to websocket.



ensure_started(Id) -> ensure_started(Id, #{}).

ensure_started(Id, Opts) ->
  % remote_ctl started as temporary
  % it does not restart when failed
  Spec = #{id => {remote_ctl, Id}, start => {remote_ctl, start_link, [Id, Opts]}, restart => transient},
  case supervisor:start_child(remote_sup, Spec) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    {error, already_present} -> {error, no_pid}
  end.



currently_running(Id) ->
  case [Pid || {{remote_ctl,Id1}, Pid, _,_} <- supervisor:which_children(remote_sup), Id1 =:= Id] of
    [] -> none;
    [Pid] -> {ok, Pid}
  end.



-record(remote_ctl, {
  id,
  node,
  last_fetched_at,
  remote_scenario_pid,
  websockets = #{} :: #{pid() => LastDeltaAt :: non_neg_integer()},

  shell_input = stopped :: {ready, binary()} | stopped
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id, Opts) ->
  gen_server:start_link(?MODULE, [Id, Opts], []).

init([Id, Opts]) ->
  Node = case maps:get(node, Opts, undefined) of
    false -> undefined;
    undefined -> self() ! connect_to_node, undefined;
    Atom when is_atom(Atom) -> self() ! connect_to_node, Atom
  end,
  {ok, #remote_ctl{id = Id, node = Node}}.



handle_info(connect_to_node, State) ->
  {ok, State1} = connect_to_node(State),
  {noreply, State1};

handle_info({events, Events}, #remote_ctl{id = Id} = State) ->
  ok = clk_events:store(Id, Events),
  {ok, State1} = fetch_and_process_events(State),
  {ok, State2} = send_delta_to_websockets(State1),
  {noreply, State2};

handle_info({shell_input, ShellInput}, #remote_ctl{} = State) ->
  {ok, State1} = send_to_websockets({shell_input, ShellInput}, State),
  {noreply, State1#remote_ctl{shell_input = ShellInput}};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(sync, _From, #remote_ctl{} = State) ->
  {reply, ok, State};

handle_call({events, Events}, _From, #remote_ctl{} = State) ->
  {noreply, State1} = handle_info({events, Events}, State),
  {reply, ok, State1};

% currently allow to subscribe only one websocket
handle_call({subscribe_websocket, Pid}, _From, #remote_ctl{} = State) ->
  {ok, State1} = subscribe_websocket(Pid, State),
  {reply, ok, State1};

handle_call(get_scenario_pid, _From, #remote_ctl{remote_scenario_pid = Pid} = State) ->
  {reply, {ok, Pid}, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



connect_to_node(#remote_ctl{node = undefined, id = Id} = State) when is_binary(Id) ->
  Dir = filename:join([code:priv_dir(batiscaph), "scenarios", Id]),
  {ok, State1} = start_local_node(Dir, State),
  connect_to_node(State1);

connect_to_node(#remote_ctl{node = Node, id = Id} = State) when Node =/= undefined andalso is_binary(Id) ->
  ok = wait_for_remote_node(Node, 5000),
  {ok, State1} = start_remote_shell(State),
  {ok, State1}.



start_local_node(DirPath, #remote_ctl{id = Id} = State) ->
  filelib:ensure_dir(DirPath),
  file:make_dir(DirPath),
  Opts = [{args, ["-noshell", "-sname", Id]}, {cd, DirPath}],
  _Port = erlang:open_port({spawn_executable, erl_exec_path()}, Opts),
  RemoteNode = list_to_atom(binary_to_list(Id) ++ "@" ++ net_adm:localhost()),
  {ok, State#remote_ctl{node = RemoteNode}}.

erl_exec_path() ->
  case os:find_executable("erl") of
    false -> "/usr/bin/erl";
    ErlPath when is_list(ErlPath) -> ErlPath
  end.



wait_for_remote_node(_RemoteNode, Timeout) when Timeout =< 0 -> {error, timeout};
wait_for_remote_node(RemoteNode, Timeout) ->
  case net_adm:ping(RemoteNode) of
    pong -> ok;
    pang ->
      timer:sleep(100),
      wait_for_remote_node(RemoteNode, Timeout - 100)
  end.



start_remote_shell(#remote_ctl{node = RemoteNode} = State) ->
  ok = remote_node:load_local_module(RemoteNode, z__client_collector),
  ok = remote_node:load_local_module(RemoteNode, z__client_io_server),
  ok = remote_node:load_local_module(RemoteNode, z__client_scenario),
  ok = remote_node:load_local_module(RemoteNode, z__client_shell),
  ok = remote_node:load_local_module(RemoteNode, z__client_sup),
  Opts = #{nodestop_on_disconnect => true, nodestop_on_scenario_shutdown => true},
  {ok, ScenarioPid} = rpc:call(RemoteNode, z__client_scenario, start_link, [node(), self(), Opts]),
  lager:info("Connected to remote shell on ~p", [RemoteNode]),
  {ok, State#remote_ctl{remote_scenario_pid = ScenarioPid}}.



subscribe_websocket(Pid, #remote_ctl{id = Id, websockets = Websockets, shell_input = ShellInput} = State) ->
  {ok, LastAt1, Delta} = produce_delta(#{instance_id => Id}),
  Pid ! {delta, Delta},
  Pid ! {shell_input, ShellInput},
  Websockets1 = Websockets#{Pid => LastAt1},
  State1 = State#remote_ctl{websockets = Websockets1},
  {ok, State1}.



send_to_websockets(Message, #remote_ctl{websockets = Websockets} = State) ->
  Websockets1 = maps:fold(fun (Pid, LastAt, Acc) ->
    case erlang:is_process_alive(Pid) of
      false -> Acc;
      true ->
        Pid ! Message,
        Acc#{Pid => LastAt}
    end
  end, #{}, Websockets),
  {ok, State#remote_ctl{websockets = Websockets1}}.



send_delta_to_websockets(#remote_ctl{id = Id, websockets = Websockets} = State) ->
  Websockets1 = maps:fold(fun (Pid, LastAt, Acc) ->
    case erlang:is_process_alive(Pid) of
      false -> Acc;
      true ->
        {ok, LastAt1, Delta} = produce_delta(#{instance_id => Id, from => LastAt}),
        Pid ! {delta, Delta},
        Acc#{Pid => LastAt1}
    end
  end, #{}, Websockets),
  {ok, State#remote_ctl{websockets = Websockets1}}.



fetch_and_process_events(#remote_ctl{id = Id} = State) ->
  case fetch_events(State) of
    {more, Events, State1} ->
      ok = n4j_processes:update(Id, Events),
      fetch_and_process_events(State1);

    {ok, Events, State1} ->
      ok = n4j_processes:update(Id, Events),
      {ok, State1}
  end.

fetch_events(#remote_ctl{last_fetched_at = LastAt, id = Id} = State) ->
  Opts = #{instance_id => Id, limit => ?MAX_EVENTS_PER_FETCH, type_in => n4j_processes:desired_event_types()},
  Opts1 = case LastAt of
    undefined -> Opts;
    _ -> Opts#{from => LastAt}
  end,

  case clk_events:select(Opts1) of
    {ok, []} -> {ok, [], State};

    {ok, Events} when length(Events) == ?MAX_EVENTS_PER_FETCH ->
      #{<<"at">> := At} = lists:last(Events),
      {more, Events, State#remote_ctl{last_fetched_at = At}};

    {ok, Events} ->
      #{<<"at">> := At} = lists:last(Events),
      {ok, Events, State#remote_ctl{last_fetched_at = At}}
  end.



produce_delta(#{} = Opts) ->
  LastAt = maps:get(from, Opts, 0),
  {ok, Delta} = delta_json(Opts),
  LastAt1 = lastest_timestamp_in_delta(LastAt, Delta),
  {ok, LastAt1, Delta}.



delta_json(Opts) ->
  ClkOpts = clickhouse_opts(Opts),
  {ok, TableEvents} = clk_events:select(ClkOpts),
  Neo4jOpts = neo4j_opts(Opts, TableEvents),
  {ok, Delta1} = n4j_processes:delta_json(Neo4jOpts),
  Delta2 = delta_with_table_events(Delta1, TableEvents),
  {ok, Delta2}.



% extract context source lines and variable bindings from clickhouse events
% and insert them into existing delta
delta_with_table_events(Delta, TableEvents) ->
  ContextLines = [E || #{<<"type">> := <<"context_start">>} = E <- TableEvents],
  VarBinds = [E || #{<<"type">> := <<"var_bind">>} = E <- TableEvents],
  ExprEvals = [E || #{<<"type">> := T} = E <- TableEvents, T == <<"expr_eval_start">> orelse T == <<"expr_eval_stop">>],
  RestEvents = [E || #{<<"type">> := T} = E <- TableEvents, T =/= <<"context_start">>, T =/= <<"var_bind">>, T =/= <<"expr_eval_start">>, T =/= <<"expr_eval_stop">>],
  #{<<"events">> := Events} = Delta,

  Delta1 = delta_with_context_lines(Delta, ContextLines),
  Delta2 = delta_with_var_binds(Delta1, VarBinds),
  ExprEvals1 = lists:sort(fun (#{<<"context">> := Ac, <<"at">> := A}, #{<<"context">> := Bc, <<"at">> := B}) ->
    {Ac, A} < {Bc, B}
  end, ExprEvals),
  Delta3 = delta_with_expr_evals(Delta2, ExprEvals1),

  Events1 = lists:sort(fun (#{<<"at">> := A}, #{<<"at">> := B}) ->
    A < B
  end, RestEvents ++ Events),
  Delta3#{<<"events">> => Events1}.



delta_with_context_lines(#{<<"contexts">> := Contexts} = Delta, ContextLines) ->
  Contexts1 = lists:foldl(fun (#{<<"context">> := C, <<"lines">> := L}, Acc) ->
    Context = maps:get(C, Acc),
    Acc#{C => Context#{<<"lines">> => jsx:decode(L)}}
  end, Contexts, ContextLines),
  Delta#{<<"contexts">> => Contexts1}.

delta_with_var_binds(#{<<"contexts">> := Contexts} = Delta, VarBinds) ->
  Contexts1 = lists:foldl(fun (#{<<"context">> := C, <<"atom">> := K, <<"term">> := V}, Acc) ->
    Context = maps:get(C, Acc),
    Binds = maps:get(<<"variables">>, Context, #{}),
    Acc#{C => Context#{<<"variables">> => Binds#{K => V}}}
  end, Contexts, VarBinds),
  Delta#{<<"contexts">> => Contexts1}.



delta_with_expr_evals(Delta, []) -> Delta;
delta_with_expr_evals(Delta, [#{<<"type">> := <<"expr_eval_start">>}]) -> Delta; % ignore unfinished expr eval
delta_with_expr_evals(Delta, [#{<<"type">> := <<"expr_eval_start">>} = Start, #{<<"type">> := <<"expr_eval_stop">>} = Stop | ExprEvals]) ->
  #{<<"at">> := StartedAt, <<"term">> := AST, <<"line">> := Line, <<"context">> := Key} = Start,
  #{<<"at">> := StoppedAt, <<"term">> := AST, <<"line">> := Line, <<"context">> := Key} = Stop,
  Result = maps:get(<<"result">>, Stop),

  #{<<"contexts">> := #{Key := Context} = Contexts} = Delta,
  Evals = maps:get(<<"evals">>, Context, #{}),
  Eval = maps:get(integer_to_binary(Line), Evals, #{<<"line">> => Line, <<"exprs">> => []}),
  Exprs = maps:get(<<"exprs">>, Eval),
  Exprs1 = Exprs ++ [#{<<"startedAt">> => StartedAt, <<"stoppedAt">> => StoppedAt, <<"result">> => Result}],
  Context1 = Context#{<<"evals">> => Evals#{integer_to_binary(Line) => Eval#{<<"exprs">> => Exprs1}}},
  delta_with_expr_evals(Delta#{<<"contexts">> => Contexts#{Key => Context1}}, ExprEvals).



table_events_types() ->
  [<<"shell_input">>,<<"shell_output">>,<<"context_start">>,<<"var_bind">>,<<"expr_eval_start">>,<<"expr_eval_stop">>].

clickhouse_opts(#{} = Opts) ->
  Opts0 = maps:with([instance_id, from, to], Opts),
  Opts0#{type_in => table_events_types()}.



neo4j_opts(#{} = Opts, TableEvents) ->
  Opts0 = maps:with([instance_id, from, to], Opts),
  Pids = unique_pids_from_events(TableEvents),
  case Pids of
    [] -> Opts0;
    [_|_] -> Opts0#{include_processes => Pids}
  end.

unique_pids_from_events(Events) ->
  unique_pids_from_events(Events, sets:new()).

unique_pids_from_events([], Set) -> sets:to_list(Set);
unique_pids_from_events([#{<<"pid">> := Pid} | Events], Set) ->
  unique_pids_from_events(Events, sets:add_element(Pid, Set)).



% FIXME I know it's ugly, but works for now,
% should be replaced with something more simple and effective
lastest_timestamp_in_delta(LastAt, #{processes := Processes, events := Events}) when is_integer(LastAt) ->
  LastAt1 = case lists:last([undefined | Events]) of
    #{<<"at">> := At1} when At1 > LastAt -> At1;
    _ -> LastAt
  end,

  ProcTimestamps = maps:fold(fun (_, #{<<"events">> := Events1} = P, Acc) ->
    EventsTimestamps = [At || #{<<"at">> := At} <- Events1],
    AppearedAt = case maps:get(<<"appearedAt">>, P, null) of
      At2 when is_integer(At2) -> At2;
      null -> LastAt
    end,
    DisappearedAt = case maps:get(<<"disappearedAt">>, P, null) of
      At3 when is_integer(At3) -> At3;
      null -> LastAt
    end,
    [lists:max([AppearedAt, DisappearedAt] ++ EventsTimestamps) | Acc]
  end, [], Processes),
  lists:max([LastAt1] ++ ProcTimestamps).
