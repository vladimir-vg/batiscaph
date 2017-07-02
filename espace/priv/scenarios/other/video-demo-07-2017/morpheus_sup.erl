-module(morpheus_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupOpts = #{strategy => one_for_one, intensity => 5, period => 1},
  Children = [
    #{id => neo_worker, start => {neo, start_link, []}}
  ],
  {ok, {SupOpts, Children}}.
