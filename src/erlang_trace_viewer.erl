-module(erlang_trace_viewer).
-export([start/0]).



start() ->
  application:ensure_all_started(erlang_trace_viewer),
  ok.