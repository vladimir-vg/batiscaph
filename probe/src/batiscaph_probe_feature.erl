-module(batiscaph_probe_feature).

-type feature_state() :: any().

% this callback accepts tracing config
% and decides should it start or not.
% If it deciddes to start then it returns new state for this feature
-callback try_init(Config :: map()) ->
  {ok, feature_state()} | skip.

% if this trace event should be processed by this feature
% then it should return true.
% To think about: this function going to be executed quite often
% it might be good idea to create at compile time one test function
% for all features.
-callback match_trace_event(TraceEvent :: tuple(), feature_state()) ->
  true | false.

% this function takes trace event
% consumes it and maybe produces some output events
-callback consume(TraceEvent :: tuple(), feature_state()) ->
  {ok, OutputEvents :: [map()], feature_state()}.

% commented out for now, return to this later:
% % this callback changes tracing state when new config provided.
% % may just stop tracing completely returning 'finished'
% -callback transition_config(OldConfig :: map(), NewConfig :: map(), feature_state()) ->
%   ok | {ok, feature_state()}.
