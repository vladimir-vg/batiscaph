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
  application:ensure_all_started(batiscaph_probe),
  {ok, no_state}.

pre_init_per_suite(Suite,Config,State) -> {Config, State}.
post_init_per_suite(_Suite,_Config,Return,State) -> {Return, State}.
pre_end_per_suite(_Suite,Config,State) -> {Config, State}.
post_end_per_suite(_Suite,_Config,Return,State) -> {Return, State}.
pre_init_per_group(_Group,Config,State) -> {Config, State}.
post_init_per_group(Group,_Config,Return, State) -> {Return, State}.
pre_end_per_group(_Group,Config,State) -> {Config, State}.
post_end_per_group(_Group,_Config,Return, State) -> {Return, State}.
pre_init_per_testcase(_TC,Config,State) -> {Config, State}.
post_end_per_testcase(TC,_Config,Return,State) -> {Return, State}.
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
