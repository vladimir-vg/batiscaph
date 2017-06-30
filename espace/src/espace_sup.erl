-module(espace_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).



start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).



init([]) ->
  % useful to cache babel output
  web_page_cache = ets:new(web_page_cache, [public, named_table, set]),

  {ok, { {one_for_all, 0, 1}, []} }.
