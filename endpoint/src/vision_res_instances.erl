-module(vision_res_instances).

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
  {UserId, Req1} = cowboy_req:qs_val(<<"user_id">>, Req),

  Ids = vision_db:query(select_instances_for_user_id, fun (C, SQL) ->
    {ok, _Cols, Rows} = epgpool:equery(C, SQL, [binary_to_integer(UserId)]),
    [Id || {Id} <- Rows]
  end),

  {ok, Instances} = vision_clk_events:select_instances_infos_with_ids(Ids),

  Body = jsx:encode(Instances),
  {Body, Req1, State}.
