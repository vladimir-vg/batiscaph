-module(vision_res_plug_requests).

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
  Req1 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, authorization">>, Req2),
  Req4 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"http://0.0.0.0:4000">>, Req3),
  {ok, Req4, no_state}.

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
  Opts = vision_delta_plug:parse_id(ReqId),
  Opts1 = Opts#{instance_id => InstanceId},
  {ok, Info} = vision_clk_events:select_plug_request_info(Opts1),
  Body = jsx:encode(Info),
  {Body, Req1, State}.
