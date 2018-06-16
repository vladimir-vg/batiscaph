-module(batiscaph_probe_cth).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).



id(_Opts) -> ?MODULE.
init(_Id, _Opts) ->
  io:format("~p init ~p ~p~n", [self(), _Id, _Opts]),
  X = application:ensure_all_started(batiscaph_probe),
  {ok, no_state}.

pre_init_per_suite(Suite,Config,State) ->
  io:format("~p pre_init_per_suite ~p ~p ~p~n", [self(), Suite,Config,State]),
  batiscaph_probe_session ! {events, [ct_event(pre_init_per_suite)]},
  {Config, State}.

post_init_per_suite(_Suite,_Config,Return,State) ->
  io:format("~p post_init_per_suite ~p ~p ~p ~p~n", [self(), _Suite,_Config,Return,State]),
  batiscaph_probe_session ! {events, [ct_event(post_init_per_suite)]},
  {Return, State}.

pre_end_per_suite(_Suite,Config,State) ->
  io:format("~p pre_end_per_suite ~p ~p ~p~n", [self(), _Suite,Config,State]),
  batiscaph_probe_session ! {events, [ct_event(pre_end_per_suite)]},
  {Config, State}.

post_end_per_suite(_Suite,_Config,Return,State) ->
  io:format("~p post_end_per_suite ~p ~p ~p ~p~n", [self(), _Suite,_Config,Return,State]),
  batiscaph_probe_session ! {events, [ct_event(post_end_per_suite)]},
  {Return, State}.

pre_init_per_group(_Group,Config,State) ->
  io:format("~p pre_init_per_group ~p ~p ~p~n", [self(), _Group,Config,State]),
  batiscaph_probe_session ! {events, [ct_event(pre_init_per_group)]},
  {Config, State}.

post_init_per_group(Group,_Config,Return, State) ->
  io:format("~p post_init_per_group ~p ~p ~p ~p~n", [self(), Group,_Config,Return,State]),
  batiscaph_probe_session ! {events, [ct_event(post_init_per_group)]},
  {Return, State}.

pre_end_per_group(_Group,Config,State) ->
  io:format("~p pre_end_per_group ~p ~p ~p~n", [self(), _Group,Config,State]),
  batiscaph_probe_session ! {events, [ct_event(pre_end_per_group)]},
  {Config, State}.

post_end_per_group(_Group,_Config,Return, State) ->
  io:format("~p post_end_per_group ~p ~p ~p ~p~n", [self(), _Group,_Config,Return,State]),
  batiscaph_probe_session ! {events, [ct_event(post_end_per_group)]},
  {Return, State}.

pre_init_per_testcase(_TC,Config,State) ->
  io:format("~p pre_init_per_testcase ~p ~p ~p~n", [self(), _TC,Config,State]),
  batiscaph_probe_session ! {events, [ct_event(pre_init_per_testcase)]},
  {Config, State}.

post_end_per_testcase(TC,_Config,Return,State) ->
  io:format("~p post_end_per_testcase ~p ~p ~p ~p~n", [self(), TC,_Config,Return,State]),
  batiscaph_probe_session ! {events, [ct_event(post_end_per_testcase)]},
  {Return, State}.

on_tc_fail({TC,_Group}, Reason, State) ->State;
on_tc_fail(TC, Reason, State) -> State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(Suite, {TC,_Group}, Reason, State) -> State;
on_tc_skip(Suite, TC, Reason, State) -> State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip({TC,Group}, Reason, State) -> State;
on_tc_skip(TC, Reason, State) -> State.

%% @doc Called when the scope of the CTH is done
terminate(_State) -> ok.



ct_event(Atom) when is_atom(Atom) ->
  #{
    at => erlang:now(),
    type => <<"p1 ct:callback">>,
    pid1 => list_to_binary(pid_to_list(self())),
    callback => atom_to_binary(Atom, latin1)
  }.
