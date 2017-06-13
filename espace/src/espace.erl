-module(espace).
-export([start/0, restart/0]).



start() ->
  application:ensure_all_started(espace),
  es_web:restart_cowboy(),
  ok.

restart() ->
  es_web:restart_cowboy(),
  ok.
