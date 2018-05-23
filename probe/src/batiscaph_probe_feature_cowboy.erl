-module(batiscaph_probe_feature_cowboy).
-behavior(batiscaph_probe_feature).
-export([try_init/1, match_trace_event/2, consume/2]).



-record(cowboy_trace, {
  handler_modules
}).



try_init(#{cowboy_requests := true})  ->
  case [1 || {ranch, _, _} <- application:which_applications()] of
    [] -> skip;
    [1] ->
      Modules = get_handler_modules(),
      cowboy_req:module_info(), % make sure module is loaded
      ok = setup_handlers_trace(Modules),
      {ok, #cowboy_trace{handler_modules = Modules}}
  end;

try_init(_) ->
  skip.



get_handler_modules() ->
  lists:usort(lists:flatmap(fun ({_Listener, Opts}) ->
    case proplists:get_value(protocol, Opts, undefined) of
      cowboy_protocol ->
        Opts1 = proplists:get_value(protocol_options, Opts, []),
        Opts2 = proplists:get_value(env, Opts1, []),
        case proplists:get_value(dispatch, Opts2, undefined) of
          undefined -> [];
          Dispatch -> get_modules_from_dispatch(Dispatch)
        end;

      _ -> []
    end
  end, ranch:info())).

get_modules_from_dispatch(Dispatch) ->
  lists:flatmap(fun ({_HostMatch, _Constraints1, Routes}) ->
    [Handler || {_Path, _Constraints2, Handler, _Opts} <- Routes]
  end, Dispatch).



setup_handlers_trace([]) -> ok;
setup_handlers_trace([M | Modules]) ->
  % make sure that modules is loaded, so meta tracer could attach to it
  M:module_info(),

  FirstIsCowboyReq =
  {'andalso', {is_tuple, '$1'},
              {'=:=', http_req,
                      {element, 1, '$1'}}},

  CowboyReqSpec = {
    ['_', '_', '_', '$1'],
    [FirstIsCowboyReq],
    []
  },
  erlang:trace_pattern({cowboy_req, reply, 4}, [CowboyReqSpec], [global]),
  erlang:trace_pattern({cowboy_req, reply, 4}, [CowboyReqSpec], [local]),

  TurnProcTrace = {trace, [], [set_on_spawn, procs, call, timestamp, {{tracer, self()}}]},
  InitCallSpec = {
    ['_', '$1', '_'],
    [FirstIsCowboyReq],
    [{return_trace}, TurnProcTrace]
  },
  erlang:trace_pattern({M, init, 3}, [InitCallSpec], [meta]),

  HandleCallSpec = {
    ['$1', '_'],
    [FirstIsCowboyReq],
    [{return_trace}]
  },
  erlang:trace_pattern({M, handle, 2}, [HandleCallSpec], [meta]),
  setup_handlers_trace(Modules).



match_trace_event({trace_ts, _, call, {_, init, [_, Req, _]}, _}, _)
when is_tuple(Req) andalso element(1, Req) =:= http_req ->
  true;
match_trace_event({trace_ts, _, return_from, {_, init, 3}, _, _}, _) ->
  true;

match_trace_event({trace_ts, _, call, {_, handle, [Req, _]}, _}, _)
when is_tuple(Req) andalso element(1, Req) =:= http_req ->
  true;
match_trace_event({trace_ts, _, return_from, {_, handle, 2}, _, _}, _) ->
  true;

match_trace_event({trace_ts, _, call, {cowboy_req, reply, [_, _, _, Req]}, _}, _)
when is_tuple(Req) andalso element(1, Req) =:= http_req ->
  true;

match_trace_event(_, _) ->
  false.



consume({trace_ts, Pid, call, {_, init, [_, Req, _]}, Timestamp}, #cowboy_trace{} = State) ->
  % right after this call we expect that this process
  % will turn on procs tracing if haven't before
  % make sure that tracing_started event is generated
  TraceEvents = batiscaph_probe_feature_procs:tracing_started_event(Pid, Timestamp),

  Method = element(6, Req),
  Host = element(9, Req),
  Port = element(11, Req),
  Path = element(12, Req),
  ReqHeaders = element(17, Req),
  Event = #{
    type => <<"p1 cowboy:request init start">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid)),
    req_headers => erlang:term_to_binary(ReqHeaders),
    path => Path, method => Method, host => Host, port => Port
  },
  {ok, [Event | TraceEvents], State};

consume({trace_ts, Pid, return_from, {_, init, 3}, _Result, Timestamp}, #cowboy_trace{} = State) ->
  Event = #{
    type => <<"p1 cowboy:request init stop">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid))
  },
  {ok, [Event], State};

consume({trace_ts, Pid, call, {_, handle, [_Req, _]}, Timestamp}, #cowboy_trace{} = State) ->
  Event = #{
    type => <<"p1 cowboy:request handle start">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid))
  },
  {ok, [Event], State};

consume({trace_ts, Pid, return_from, {_, handle, 2}, _Result, Timestamp}, #cowboy_trace{} = State) ->
  Event = #{
    type => <<"p1 cowboy:request handle stop">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid))
  },
  {ok, [Event], State};

consume({trace_ts, Pid, call, {cowboy_req, reply, [RespCode, RespHeaders, Body, _Req]}, Timestamp}, #cowboy_trace{} = State) ->
  Event = #{
    type => <<"p1 cowboy:request reply">>,
    at => Timestamp,
    pid1 => list_to_binary(pid_to_list(Pid)),
    resp_code => RespCode,
    resp_headers => erlang:term_to_binary(RespHeaders),
    resp_body_size => erlang:iolist_size(Body)
  },
  {ok, [Event], State};

consume(_E, #cowboy_trace{} = State) ->
  % io:format("cowboy trace: ~p~n", [E]),
  {ok, [], State}.



