-module(vision_cmds).
-export([reset_clickhouse/0]).



% functions in this module expected to be called from outside
% without starting whole app



reset_clickhouse() ->
  {ok, _} = application:ensure_all_started(hackney),
  ok = vision_app:read_config(),

  try vision_clk_events:drop_tables()
  catch _:_ -> ok
  end,

  ok = vision_clk_events:create_tables(),
  ok.
