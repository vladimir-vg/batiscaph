-module(example_app1_app).
-behaviour(application).
-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
  example_app1_sup:start_link().

stop(_State) ->
  ok.
