-module(delta_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).



% This suite start batiscaph, sends list of events
% and checks that resulting delta is correct



all() ->
  [].



init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.
