-module(z__remote_scenario).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ESPACE_NODE_TIMEOUT, 5000).



%%% This module does node-to-node communication



-record(scenario, {
  opts,
  espace_node,
  espace_node_timer,
  receiver_pid,

  % supervisor monitor ref
  sup_mref
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(ESpaceNode, ReceiverPid, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ESpaceNode, ReceiverPid, Opts], []).

init([ESpaceNode, ReceiverPid, Opts]) ->
  self() ! setup,
  {ok, #scenario{espace_node = ESpaceNode, receiver_pid = ReceiverPid, opts = Opts}}.



handle_info(setup, State) ->
  {ok, State1} = setup(State),
  {noreply, State1};

handle_info(check_espace_node, #scenario{espace_node = ESpaceNode} = State) ->
  ok = check_espace_node(ESpaceNode),
  {ok, State1} = refresh_espace_node_timer(State),
  {noreply, State1};

handle_info({'DOWN', MRef, process, _Pid, Reason}, #scenario{sup_mref = MRef} = State) ->
  % stop node if we were monitoring supervisor and it died
  io:format("Scenario supervisor died, shutdown node: ~p\n", [Reason]),
  timer:sleep(1000),
  init:stop(),
  {noreply, State};

handle_info(shell_restart, State) ->
  ok = gen_server:call(z__remote_shell, restart_shell),
  {noreply, State};

handle_info({store_module, Name, Body}, State) ->
  {ok, State1} = store_module(Name, Body, State),
  {noreply, State1};

handle_info({shell_input, Input}, State) ->
  z__remote_io_server ! {input, binary_to_list(Input)},
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(get_remote_receiver_pid, _From, #scenario{receiver_pid = ReceiverPid} = State) ->
  {reply, {ok, ReceiverPid}, State};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



refresh_espace_node_timer(#scenario{espace_node_timer = Timer} = State) when Timer =/= undefined ->
  erlang:cancel_timer(Timer),
  refresh_espace_node_timer(State#scenario{espace_node_timer = undefined});

refresh_espace_node_timer(#scenario{espace_node_timer = undefined} = State) ->
  Timer = erlang:send_after(?ESPACE_NODE_TIMEOUT, self(), check_espace_node),
  {ok, State#scenario{espace_node_timer = Timer}}.



check_espace_node(ESpaceNode) ->
  case net_adm:ping(ESpaceNode) of
    pong -> ok;
    pang ->
      % remote espace is unavailable, shut down node
      io:format("Espace node is gone, shutting down\n"),
      timer:sleep(1000),
      init:stop()
  end.



setup(#scenario{opts = Opts} = State) ->
  {ok, SupPid} = z__remote_sup:start_link(),

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
      {ok, State2} = refresh_espace_node_timer(State1),
      {ok, State2}
  end.



store_module(Name, Body, #scenario{} = State) ->
  Event = module_stored_event_now(Name, Body),
  Event1 = case file:write_file(<<Name/binary, ".erl">>, Body) of
    ok -> Event;
    {error, Reason} ->
      Event#{type => <<"module_storage_failed">>, term => iolist_to_binary(io_lib:format("~p", [Reason]))}
  end,
  z__remote_collector ! Event1,
  {ok, State}.

module_stored_event_now(Name, Body) ->
  Now = erlang:system_time(micro_seconds),
  #{
    <<"at">> => (Now div (1000*1000)),
    <<"at_mcs">> => (Now rem (1000*1000)),
    <<"type">> => <<"module_stored">>,
    <<"atom">> => Name,
    <<"size">> => byte_size(Body),
    <<"hash">> => bin_to_hex:bin_to_hex(crypto:hash(md5, Body))
  }.
