-module(espace).
-export([get_prop/1, create_tables/0, drop_tables/0]).



create_tables() ->
  ok = clk_events:create_table(),
  ok.

drop_tables() ->
  ok = clk_events:drop_table(),
  ok.



get_prop(Name) ->
  case erlang:get({app_config_prop, Name}) of
    undefined ->
      {ok, Value} = application:get_env(espace, Name),
      put({app_config_prop, Name}, Value),
      Value;
    Value -> Value
  end.