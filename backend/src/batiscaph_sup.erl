-module(batiscaph_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Children = [
    #{id => probes, start => {gen_tracker, start_link, [probes]}, type => supervisor, shutdown => infinity},
    #{id => delta_producers, start => {gen_tracker, start_link, [delta_producers]}, type => supervisor, shutdown => infinity}
    % #{id => batiscaph_delta_sup, start => {batiscaph_delta_sup, start_link, []}, type => supervisor, shutdown => infinity}
  ],
  {ok, {{one_for_all, 5, 1}, Children}}.
