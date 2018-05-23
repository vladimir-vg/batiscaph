-module(batiscaph_probe_feature_lager).
-behavior(batiscaph_probe_feature).
-export([try_init/1, match_trace_event/2, consume/2]).



try_init(_) ->
  case code:is_loaded(lager_app) of
    false -> skip;
    {file, _} ->
      case lists:member({start_handler,3}, lager_app:module_info(exports)) of
        false -> skip;
        true ->
          lager_app:start_handler(lager_event, batiscaph_probe_lager_backend, []),
          {ok, no_state}
      end
  end.



match_trace_event(_, _) ->
  false.



consume(_E, State) ->
  {ok, [], State}.


