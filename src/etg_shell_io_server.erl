-module(etg_shell_io_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(shell_io, {
  collector_pid
}).



code_change(_, State, _) -> {ok, State}.
terminate(_,_State) -> ok.

start_link(CollectorPid) ->
  gen_server:start_link(?MODULE, [CollectorPid], []).

init([CollectorPid]) ->
  {ok, #shell_io{collector_pid=CollectorPid}}.



handle_info({execute, Statement}, State) ->
  io:format("must execute: ~p~n", [Statement]),
  {noreply, State};

handle_info({io_request, From, ReplyAs, Request}, State) ->
  {ok, State1} = handle_io_request(From, ReplyAs, Request, State),
  {noreply, State1};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.



handle_io_request(From, ReplyAs, {put_chars,unicode,io_lib,format,[Format,Args]}, #shell_io{collector_pid=CollectorPid} = State) ->
  Now = erlang:system_time(micro_seconds),
  Output = io_lib:format(Format, Args),
  Event = #{
    at => (Now div (1000*1000)),
    at_mcs => (Now rem (1000*1000)),
    type => <<"shell_output">>,
    message => Output
  },
  CollectorPid ! Event,
  io:format("io_request: ~p~n", [Event]),
  From ! {io_reply, ReplyAs, ok},
  {ok, State};

handle_io_request(_From, _ReplyAs, Request, State) ->
  io:format("unknown_request: ~p~n", [Request]),
  {ok, State}.
