-module(old_batiscaph_receiver).
-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-record(receiver, {
  id
}).



start(Id) ->
  Spec = #{id => {receiver,Id}, start => {?MODULE, start_link, [Id]}},
  case supervisor:start_child(batiscaph_sup, Spec) of
    {ok, Pid} -> {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.




code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
  {ok, #receiver{id = Id}}.



handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.



handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.



handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
