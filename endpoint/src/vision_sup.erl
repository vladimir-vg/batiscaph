-module(vision_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Children = [
    % #{id => remote_sup, start => {remote_sup, start_link, []}, type => supervisor, shutdown => infinity}
  ],
  {ok, {{one_for_all, 5, 1}, Children}}.
