-module(vision_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Children = [
    #{id => probes, start => {gen_tracker, start_link, [probes]}, type => supervisor, shutdown => infinity}
  ],
  {ok, {{one_for_all, 5, 1}, Children}}.
