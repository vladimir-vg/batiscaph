-module(batiscaph_res_cowboy_requests).

% Cowboy callbacks
-export([
  init/3, rest_init/2, terminate/3, allowed_methods/2,
  content_types_provided/2
  % resource_exists/2
]).

% our response callbacks
-export([
  get_json/2
]).



init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  Types = [
    {<<"application/json">>, get_json}
  ],
  {Types, Req, State}.



get_json(Req, State) ->
  {ReqId, Req1} = cowboy_req:binding(req_id, Req),
  {InstanceId, Req1} = cowboy_req:binding(instance_id, Req),
  Opts = batiscaph_delta_cowboy:parse_id(ReqId),
  Opts1 = Opts#{instance_id => InstanceId},
  {ok, Info} = batiscaph_events:select_cowboy_request_info(Opts1),
  Body = jsx:encode(Info),
  {Body, Req1, State}.
