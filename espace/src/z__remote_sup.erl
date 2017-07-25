-module(z__remote_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Children = [
    #{id => z__remote_io_server, start => {z__remote_io_server, start_link, []}},
    #{id => z__remote_collector, start => {z__remote_collector, start_link, []}},
    #{id => z__remote_shell, start => {z__remote_shell, start_link, []}}
  ],
  {ok, {{one_for_all, 1, 10}, Children}}.
