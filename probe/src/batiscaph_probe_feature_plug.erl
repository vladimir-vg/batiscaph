-module(batiscaph_probe_feature_plug).
-behavior(batiscaph_probe_feature).
-export([try_init/1, match_trace_event/2, consume/2]).



-record(phoenix_trace, {
  endpoint_modules,

  % #{ pid() => true }, just to check if process in the middle of request
  % to filter out wrong plug calls
  ongoing_reqs = #{}
}).



try_init(#{plug_requests := true})  ->
  Apps = apps_that_depend_on_phoenix(),
  EndpointModules = endpoints_in_apps(Apps),
  case EndpointModules of
    [] -> skip; % no endpoints were found
    _ ->
      ok = trace_http_requests(EndpointModules),
      {ok, #phoenix_trace{endpoint_modules = EndpointModules}}
  end;

try_init(_) ->
  skip.



apps_that_depend_on_phoenix() ->
  Apps = batiscaph_probe_util:apps_have_dependency(phoenix),
  [Atom || {Atom, _, _} <- Apps].

endpoints_in_apps(Apps) ->
  % for now just rely on modules specified in application
  lists:foldl(fun (AppId, Acc) ->
    case application:get_key(AppId, modules) of
      undefined -> Acc;
      {ok, Modules} ->
        Modules1 = lists:filter(fun is_endpoint_module_name/1, Modules),
        Modules1 ++ Acc
    end
  end, [], Apps).

% simply and stupidly match for 'Elixir.*.Endpoint' name
is_endpoint_module_name(Module) ->
  case binary:split(atom_to_binary(Module, latin1), <<".">>, [global]) of
    [_] -> false;
    Parts ->
      case {hd(Parts), lists:last(Parts)} of
        {<<"Elixir">>, <<"Endpoint">>} -> true;
        _ -> false
      end
  end.



trace_http_requests(EndpointModules) ->
  % because any 2-arity function might be used as a plug
  % and there is no way to match content of map argument
  % in much spec, we have to make very generic match
  % and receive some redudant call traces

  PlugLikeSpec = {
    ['$1', '$2'],
    [{is_map, '$1'}],
    [{return_trace}]
  },

  % specify that we want to receive all calls that look like
  % plug calls. But these calls would be delivered when
  % [call] trace is activated by meta tracer
  %
  % currently erlang don't take {'_', call, 2} pattern
  % Specify very generic pattern and then filter by hand.
  % This is not good for performance
  %
  % TODO: look after all loaded/unloaded modules, check which have exported call/2
  % and call trace_pattern for them
  % TODO: actually this can be wonderfully filtered using erl_tracer
  % without any copying
  erlang:trace_pattern({'_', '_', '_'}, [PlugLikeSpec], [global]),

  % many plugs that gonna be used might be not loaded into system yet
  % enable tracing 'on_load'
  erlang:trace_pattern(on_load, [PlugLikeSpec], [global]),

  % receiving endpoint call turns on call tracing,
  % so all plug-like calls will be traced, like:
  %     erlang:trace(Pid, true, [call])
  %
  % should explicitly specify tracer pid, because likely that process
  % is not traced yet, and doesn't know where to send traces
  TurnCallTrace = {trace, [], [set_on_spawn, procs, call, timestamp, {{tracer, self()}}]},

  PlugLikeSpec1 = {
    ['$1', '$2'],
    [{is_map, '$1'}],
    [{return_trace}, TurnCallTrace]
  },
  [erlang:trace_pattern({M, call, 2}, [PlugLikeSpec1], [meta]) || M <- EndpointModules],

  ok.



% for now capture only module-based plugs, that use 'call' function
% skip everything else

match_trace_event({trace_ts, _, call, {_, call, [Conn, _]}, _}, _) when is_map(Conn) ->
  maps:get('__struct__', Conn, undefined) =:= 'Elixir.Plug.Conn';

match_trace_event({trace_ts, _, return_from, {_, call, 2}, Result, _}, _) when is_map(Result) ->
  maps:get('__struct__', Result, undefined) =:= 'Elixir.Plug.Conn';

match_trace_event(_, _) ->
  false.



% lists:member(Module, EndpointModules)
consume({trace_ts, Pid, call, {Module, call, _} = MFA, Timestamp}, #phoenix_trace{endpoint_modules = EndpointModules} = State) ->
  % right after this call we expect that this process
  % will turn on procs tracing if haven't before
  % make sure that tracing_started event is generated
  TraceEvents = batiscaph_probe_feature_procs:tracing_started_event(Pid, Timestamp),
  {ok, Events, State1} =
    case lists:member(Module, EndpointModules) of
      true -> start_request(Pid, MFA, Timestamp, State);
      false -> start_plug(Pid, MFA, Timestamp, State)
    end,
  {ok, TraceEvents ++ Events, State1};

consume({trace_ts, Pid, return_from, {Module, call, 2}, Result, Timestamp}, #phoenix_trace{endpoint_modules = EndpointModules} = State) ->
  case lists:member(Module, EndpointModules) of
    true -> stop_request(Pid, Result, Timestamp, State);
    false -> stop_plug(Pid, Module, Result, Timestamp, State)
  end.



start_request(Pid, {_, call, [Conn, _]}, Timestamp, #phoenix_trace{ongoing_reqs = ReqsMap} = State) ->
  #{
    request_path := Path, method := Method,
    host := Host, port := Port, req_headers := ReqHeaders
  } = Conn,
  Event = #{
    type => <<"p1 plug:request start">>, at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid)),
    req_headers => erlang:term_to_binary(ReqHeaders),
    path => Path, method => Method, host => Host, port => Port
  },
  State1 = State#phoenix_trace{ongoing_reqs = maps:put(Pid, true, ReqsMap)},
  {ok, [Event], State1}.

stop_request(Pid, Conn, Timestamp, #phoenix_trace{ongoing_reqs = ReqsMap} = State) ->
  #{
    request_path := Path, method := Method,
    host := Host, port := Port, resp_headers := RespHeaders,
    status := Code
  } = Conn,
  ReqId = proplists:get_value(<<"x-request-id">>, RespHeaders, <<>>),
  Event = #{
    type => <<"p1 plug:request stop">>, at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid)),

    resp_headers => erlang:term_to_binary(RespHeaders),
    path => Path, method => Method, host => Host, port => Port,
    request_id => ReqId, resp_code => Code
  },
  State1 = State#phoenix_trace{ongoing_reqs = maps:remove(Pid, ReqsMap)},
  {ok, [Event], State1}.



start_plug(Pid, {Module, call, [Conn, _]}, Timestamp, #phoenix_trace{ongoing_reqs = ReqsMap} = State) ->
  case maps:get(Pid, ReqsMap, undefined) of
    undefined -> {ok, [], State};
    true ->
      Halted = maps:get(halted, Conn, false),
      Event = #{
        type => <<"p1 plug:plug start">>, at => Timestamp,
        pid1 => list_to_binary(pid_to_list(Pid)),
        module => atom_to_binary(Module, latin1), halted => Halted
      },
      {ok, [Event], State}
  end.

stop_plug(Pid, Module, Conn, Timestamp, #phoenix_trace{ongoing_reqs = ReqsMap} = State) ->
  case maps:get(Pid, ReqsMap, undefined) of
    undefined -> {ok, [], State};
    true ->
      Halted = maps:get(halted, Conn, false),
      Event = #{
        type => <<"p1 plug:plug stop">>, at => Timestamp,
        pid1 => list_to_binary(pid_to_list(Pid)),
        module => atom_to_binary(Module, latin1), halted => Halted
      },
      {ok, [Event], State}
  end.

