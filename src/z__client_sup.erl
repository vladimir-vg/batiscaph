-module(z__client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Children = [
    #{id => z__client_io_server, start => {z__client_io_server, start_link, []}},
    #{id => z__client_collector, start => {z__client_collector, start_link, []}},
    #{id => z__client_shell, start => {z__client_shell, start_link, []}}
  ],
  {ok, {{one_for_all, 1, 10}, Children}}.
