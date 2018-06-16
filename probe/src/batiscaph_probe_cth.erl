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
-export([post_init_per_testcase/4]).
-export([pre_end_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).



id(_Opts) -> ?MODULE.
init(_Id, _Opts) ->
  application:ensure_all_started(batiscaph_probe),
  {ok, no_state}.



pre_init_per_suite(Suite,Config,State) ->
  E = ct_callback_event(pre_init_per_suite, #{suite => atom_to_binary(Suite,latin1)}),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_init_per_suite(Suite,_Config,Return,State) ->
  E = ct_callback_event(post_init_per_suite, #{suite => atom_to_binary(Suite,latin1)}),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.

pre_end_per_suite(Suite,Config,State) ->
  E = ct_callback_event(pre_end_per_suite, #{suite => atom_to_binary(Suite,latin1)}),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_end_per_suite(Suite,_Config,Return,State) ->
  E = ct_callback_event(post_end_per_suite, #{suite => atom_to_binary(Suite,latin1)}),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.



pre_init_per_group(_Group,Config,State) ->
  E = ct_callback_event(pre_init_per_group, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_init_per_group(_Group,Config,Return, State) ->
  E = ct_callback_event(post_init_per_group, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.

pre_end_per_group(_Group,Config,State) ->
  E = ct_callback_event(pre_end_per_group, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_end_per_group(_Group,Config,Return, State) ->
  E = ct_callback_event(post_end_per_group, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.



pre_init_per_testcase(TC,Config,State) ->
  E = ct_callback_event(pre_init_per_testcase, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config),
    testcase => atom_to_binary(TC,latin1)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_init_per_testcase(TC,Config,Return,State) ->
  E = ct_callback_event(post_init_per_testcase, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config),
    testcase => atom_to_binary(TC,latin1)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.

pre_end_per_testcase(TC,Config,State) ->
  E = ct_callback_event(pre_end_per_testcase, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config),
    testcase => atom_to_binary(TC,latin1)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Config, State}.

post_end_per_testcase(TC,Config,Return,State) ->
  E = ct_callback_event(post_end_per_testcase, #{
    groups => groups_from_config(Config),
    suite => suite_from_config(Config),
    testcase => atom_to_binary(TC,latin1)
  }),
  batiscaph_probe_session ! {events, [E]},
  {Return, State}.



on_tc_fail({_TC,_Group}, _Reason, State) -> State;
on_tc_fail(_TC, _Reason, State) -> State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(_Suite, {_TC,_Group}, _Reason, State) -> State;
on_tc_skip(_Suite, _TC, _Reason, State) -> State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip({_TC,_Group}, _Reason, State) -> State;
on_tc_skip(_TC, _Reason, State) -> State.

%% @doc Called when the scope of the CTH is done
terminate(_State) -> ok.



suite_from_config(Config) ->
  Props = proplists:get_value(tc_group_properties, Config),
  atom_to_binary(proplists:get_value(suite, Props), latin1).



groups_from_config(Config) ->
  Props = proplists:get_value(tc_group_properties, Config),
  Path = proplists:get_value(tc_group_path, Config),
  Groups = lists:reverse(lists:flatten(
    get_group(Props) ++ [get_group(Part) || Part <- Path])),
  iolist_to_binary(lists:join(<<" ">>, [atom_to_binary(A,latin1) || A <- Groups])).

get_group(Part) ->
  case proplists:get_value(name, Part, undefined) of
    undefined -> [];
    Group when is_atom(Group) -> [Group]
  end.



ct_callback_event(Atom, Attrs) when is_atom(Atom) andalso is_map(Attrs) ->
  Attrs#{
    at => erlang:now(),
    type => <<"p1 ct:callback">>,
    pid1 => list_to_binary(pid_to_list(self())),
    callback => atom_to_binary(Atom, latin1)
  }.
