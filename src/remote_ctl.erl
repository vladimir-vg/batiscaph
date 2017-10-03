-module(remote_ctl).
-behaviour(gen_server).
-export([ensure_started/1, ensure_started/2]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAX_EVENTS_PER_FETCH, 1000).



%%% This process accept events from remote node
%%% and stores it to database. Sends prepared events to websocket.



ensure_started(Id) -> ensure_started(Id, undefined).

ensure_started(Id, Node) ->
  % remote_ctl started as temporary
  % it does not restart when failed
  Spec = #{id => {remote_ctl, Id}, start => {remote_ctl, start_link, [Id, Node]}, restart => temporary},
  case supervisor:start_child(remote_sup, Spec) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    {error, already_present} -> {error, no_pid}
  end.



-record(remote_ctl, {
  id,
  node,
  last_fetched_at,
  remote_scenario_pid,
  websockets = #{} :: #{pid() => LastDeltaAt :: non_neg_integer()}
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id, Node) ->
  gen_server:start_link(?MODULE, [Id, Node], []).

init([Id, Node]) ->
  self() ! connect_to_node,
  {ok, #remote_ctl{id = Id, node = Node}}.



handle_info(connect_to_node, State) ->
  {ok, State1} = connect_to_node(State),
  {noreply, State1};

handle_info({events, Events}, #remote_ctl{id = Id} = State) ->
  ok = clk_events:store(Id, Events),
  {ok, State1} = fetch_and_process_events(State),
  {ok, State2} = send_delta_to_websockets(State1),
  {noreply, State2};

handle_info({shell_input_ready, Prompt}, #remote_ctl{} = State) ->
  {ok, State1} = send_to_websockets({shell_input_ready, Prompt}, State),
  {noreply, State1};

handle_info(shell_input_stopped, #remote_ctl{} = State) ->
  {ok, State1} = send_to_websockets(shell_input_stopped, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



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
  Dir = filename:join([code:priv_dir(espace), "scenarios", Id]),
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
  ok = remote_node:load_local_module(RemoteNode, z__remote_collector),
  ok = remote_node:load_local_module(RemoteNode, z__remote_io_server),
  ok = remote_node:load_local_module(RemoteNode, z__remote_scenario),
  ok = remote_node:load_local_module(RemoteNode, z__remote_shell),
  ok = remote_node:load_local_module(RemoteNode, z__remote_sup),
  Opts = #{nodestop_on_disconnect => true, nodestop_on_scenario_shutdown => true},
  {ok, ScenarioPid} = rpc:call(RemoteNode, z__remote_scenario, start_link, [node(), self(), Opts]),
  lager:info("Connected to remote shell on ~p", [RemoteNode]),
  {ok, State#remote_ctl{remote_scenario_pid = ScenarioPid}}.



subscribe_websocket(Pid, #remote_ctl{id = Id, websockets = Websockets} = State) ->
  {ok, LastAt1, Delta} = produce_delta(Id, 0),
  Pid ! {delta, Delta},
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
        {ok, LastAt1, Delta} = produce_delta(Id, LastAt),
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
    _ -> Opts#{'after' => LastAt}
  end,

  case clk_events:select(Opts1) of
    {ok, []} -> {ok, [], State};

    {ok, Events} when length(Events) == ?MAX_EVENTS_PER_FETCH ->
      #{<<"at_s">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {more, Events, State#remote_ctl{last_fetched_at = {At, Mcs}}};

    {ok, Events} ->
      #{<<"at_s">> := At, <<"at_mcs">> := Mcs} = lists:last(Events),
      {ok, Events, State#remote_ctl{last_fetched_at = {At, Mcs}}}
  end.



produce_delta(Id, LastAt) ->
  {ok, Delta} = delta_json(Id, LastAt),
  LastAt1 = lastest_timestamp_in_delta(LastAt, Delta),
  {ok, LastAt1, Delta}.



delta_json(Id, LastAt) ->
  ClkOpts = clickhouse_opts(Id, LastAt),
  lager:info("---------------------------: ~p", [ClkOpts]),
  {ok, TableEvents} = clk_events:select(ClkOpts),
  Neo4jOpts = neo4j_opts(Id, LastAt, TableEvents),
  lager:info("---------------------------: ~p", [Neo4jOpts]),
  {ok, #{processes := Processes, events := GraphEvents}} = n4j_processes:delta_json(Neo4jOpts),
  Delta = #{processes => Processes, graph_events => GraphEvents, table_events => TableEvents},
  {ok, Delta}.



table_events_types() ->
  [<<"shell_input">>,<<"shell_output">>].

clickhouse_opts(Id, LastAt) ->
  #{instance_id => Id, 'after' => LastAt, type_in => table_events_types()}.



neo4j_opts(Id, LastAt, TableEvents) ->
  Opts0 = #{instance_id => Id, 'after' => LastAt},
  Pids = [P || #{<<"pid">> := P} <- TableEvents],
  case Pids of
    [] -> Opts0;
    [_|_] -> Opts0#{include_processes => Pids}
  end.



% FIXME I know it's ugly, but works for now,
% should be replaced with something more simple and effective
lastest_timestamp_in_delta(LastAt, #{processes := Processes, table_events := TableEvents, graph_events := GraphEvents}) when is_integer(LastAt) ->
  LastAt1 = case lists:last([undefined | TableEvents]) of
    #{<<"at">> := At1} when At1 > LastAt -> At1;
    _ -> LastAt
  end,
  LastAt2 = case lists:last([undefined | GraphEvents]) of
    #{<<"at">> := At4} when At4 > LastAt1 -> At4;
    _ -> LastAt1
  end,
  ProcTimestamps = lists:map(fun (#{<<"events">> := Events1} = P) ->
    EventsTimestamps = [At || #{<<"at">> := At} <- Events1],
    AppearedAt = case maps:get(<<"appearedAt">>, P, null) of
      At2 when is_integer(At2) -> At2;
      null -> LastAt
    end,
    DisappearedAt = case maps:get(<<"disappearedAt">>, P, null) of
      At3 when is_integer(At3) -> At3;
      null -> LastAt
    end,
    lists:max([AppearedAt, DisappearedAt] ++ EventsTimestamps)
  end, Processes),
  lists:max([LastAt2] ++ ProcTimestamps).
