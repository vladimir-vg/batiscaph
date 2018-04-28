-module(batiscaph_handler_probe).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  case what_kind_of_request(Req) of
    {unknown, Req1} ->
      {ok, Req2} = cowboy_req:reply(400, Req1),
      {ok, Req2, bad_request};

    {guest_info, Ip, Info, Req1} ->
      State = {guest_info, Ip, Info},
      {ok, Req1, State};

    {upgrade_to_persistent, _Req1} ->
      {upgrade, protocol, batiscaph_probe_protocol}
  end.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, bad_request) ->
  {ok, Req, bad_request};

handle(Req, {guest_info, _Ip, Info} = State) ->
  ok = batiscaph_test:notify_guest_info_subscribers(Info),
  {ok, Req1} = cowboy_req:reply(200, Req),
  {ok, Req1, State}.


% this method determines how to work with this connection further
% is it just a short guest push, or a persistent connection
% for authorized user
what_kind_of_request(Req) ->
  case cowboy_req:header(<<"content-type">>, Req) of
    {<<"application/batiscaph-guest-info-v0">>, Req1} ->
      {ok, Body, Req2} = cowboy_req:body(Req1),
      {{Ip, _Port}, Req3} = cowboy_req:peer(Req2),
      {guest_info, Ip, erlang:binary_to_term(Body), Req3};

    {undefined, Req1} ->
      case cowboy_req:header(<<"upgrade">>, Req1) of
        {<<"application/batiscaph-persistent-v0">>, Req2} ->
          {upgrade_to_persistent, Req2};

        {undefined, Req2} ->
          {unknown, Req2}
      end
  end.

