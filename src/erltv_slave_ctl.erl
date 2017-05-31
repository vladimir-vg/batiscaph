-module(erltv_slave_ctl).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(slave_ctl, {
  dir,
  id,
  port
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(DateDir, Id) ->
  gen_server:start_link(?MODULE, [DateDir, Id], []).

init([DateDir, Id]) ->
  self() ! start_slave,
  {ok, #slave_ctl{dir = DateDir, id = Id}}.



handle_info(start_slave, State) ->
  {ok, State1} = start_slave(State),
  {noreply, State1};

handle_info({Port, Msg}, #slave_ctl{port = Port} = State) ->
  lager:info("slave message: ~p", [Msg]),
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



start_slave(#slave_ctl{dir = Dir, id = Id} = State) ->
  Erl = "/usr/bin/erl", % TODO: discover where erl is localted
  {ok, MasterPort} = application:get_env(erltv, http_port),
  Paths = string:tokens(os:cmd("./rebar3 path"), " "),
  Args0 = lists:concat([["-pa", P] || P <- Paths]),
  Args1 = Args0 ++ ["-noshell", "-sname", Id, "-s", "erltv_slave"],
  Env = [{"MASTER_PORT", integer_to_list(MasterPort)}, {"SHELL_SESSION_ID", binary_to_list(Id)}],
  DirPath = filename:join([code:priv_dir(erltv), "scenarios", Dir, Id]),
  ok = filelib:ensure_dir(DirPath),
  ok = file:make_dir(DirPath),
  Opts = [{args, Args1},{env,Env},{cd,DirPath}],
  Port = erlang:open_port({spawn_executable, Erl}, Opts),
  {ok, State#slave_ctl{port = Port}}.