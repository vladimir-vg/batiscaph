-module(batiscaph_probe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  % start supervisor only if endpoint_url was provided
  % otherwise just start empty supervisor

  Children =
    case application:get_env(batiscaph_probe, endpoint_url) of
      undefined -> [];
      {ok, _} ->
        [
          {batiscaph_probe_session, {batiscaph_probe_session, start_link, []}, permanent, 5000, worker, [batiscaph_probe_session]},
          {batiscaph_probe_trace_collector, {batiscaph_probe_trace_collector, start_link, []}, permanent, 5000, worker, [batiscaph_probe_trace_collector]},
          {batiscaph_probe_subs_worker, {batiscaph_probe_subs_worker, start_link, []}, permanent, 5000, worker, [batiscaph_probe_subs_worker]}
        ]
    end,

  {ok, {{one_for_all, 1, 10}, Children}}.
