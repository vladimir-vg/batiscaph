-module(batiscaph_cmds).
-export([reset_storage/0]).



% functions in this module expected to be called from outside
% without starting whole app



reset_storage() ->
  {ok, _} = application:ensure_all_started(hackney),
  ok = batiscaph_app:read_config(),

  try batiscaph_events_clickhouse:drop_tables()
  catch _:_ -> ok
  end,

  ok = batiscaph_events_clickhouse:create_tables(),
  ok.
