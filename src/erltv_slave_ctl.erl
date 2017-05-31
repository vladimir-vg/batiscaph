-module(erltv_slave_ctl).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(slave_ctl, {
  dir,
  id
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

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



start_slave(#slave_ctl{dir = _Dir, id = _Id} = State) ->
  % here we should start slave application
  % and later communicate with it over tcp
  {ok, State}.