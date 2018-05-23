-module(batiscaph_probe_feature_procs).
-behavior(batiscaph_probe_feature).
-export([try_init/1, match_trace_event/2, consume/2]).
-export([tracing_started_event/2]).



try_init(_) ->
  {ok, no_state}.



match_trace_event({trace_ts, _, spawn, _, _, _}, _) -> true;
% match_trace_event({trace_ts, _, spawned, _, _, _}, _) -> true;
match_trace_event({trace_ts, _, exit, _, _}, _) -> true;
match_trace_event(_, _) ->
  false.



consume({trace_ts, ParentPid, spawn, ChildPid, MFA, Timestamp}, State) ->
  Event = #{
    type => <<"p2 erlang:process spawn">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(ParentPid)),
    pid2 => list_to_binary(pid_to_list(ChildPid)),
    mfa => batiscaph_probe_util:format_mfa(MFA)
  },
  % io:format("procs trace: ~p~n", [Event]),
  {ok, [Event], State};

consume({trace_ts, Pid, exit, Reason, Timestamp}, State) ->
  Event = #{
    type => <<"p1 erlang:process exit">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid)),
    reason => batiscaph_probe_util:format_reason(Reason)
  },
  {ok, [Event], State}.



% TODO: find a right module for this function
% prefer to keep only trace events processing code here
% TODO: remove dead pids from table from time to time
tracing_started_event(Pid, Timestamp) ->
  case ets:lookup(batiscaph_probe_procs, Pid) of
    [{Pid, _}] -> [];
    [] ->
      true = ets:insert_new(batiscaph_probe_procs, {Pid, Timestamp}),
      Event = #{
        type => <<"p1 erlang:process trace start">>,
        at => Timestamp,
        pid1 => list_to_binary(pid_to_list(Pid))
      },
      [Event]
  end.

