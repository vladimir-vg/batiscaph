-module(erlang19_app1_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  {ok, {{one_for_all, 0, 1}, []}}.
