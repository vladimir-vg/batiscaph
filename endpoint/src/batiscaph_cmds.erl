-module(batiscaph_cmds).
-export([reset_clickhouse/0]).



% functions in this module expected to be called from outside
% without starting whole app



reset_clickhouse() ->
  {ok, _} = application:ensure_all_started(hackney),
  ok = batiscaph_app:read_config(),

  try batiscaph_clk_events:drop_tables()
  catch _:_ -> ok
  end,

  ok = batiscaph_clk_events:create_tables(),
  ok.
