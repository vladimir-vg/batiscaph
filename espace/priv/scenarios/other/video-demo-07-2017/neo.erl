-module(neo).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, no_state}.


handle_info(Msg, State) ->
  {stop, {unknown_message, Msg}, State}.


handle_call(find_smith, _From, State) ->
  {reply, {found, hinding_under_the_table}, State};
handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.


handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.


code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.