-module(erlang_trace_viewer_app).
-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
  erlang_trace_viewer_sup:start_link().



stop(_State) ->
  ok.

