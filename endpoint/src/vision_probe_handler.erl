-module(vision_probe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  case check_method(Req) of
    {bad, Req1} -> {ok, Req1, State};
    {ok, Req1} ->
      case what_kind_of_request(Req1) of
        {guest_info, _Ip, Info, Req2} ->
          ok = vision_test:notify_guest_info_subscriber(Info),
          {ok, Req3} = cowboy_req:reply(200, Req2),
          {ok, Req3, State}
      end
  end.

check_method(Req) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, Req1} -> {ok, Req1};
    {_, Req1} ->
      {ok, Req2} = cowboy_req:reply(405, Req1),
      {bad, Req2}
  end.



% this method determines how to work with this connection further
% is is just a short guest push, or a persistent connection
% for authorized user
what_kind_of_request(Req) ->
  case cowboy_req:header(<<"content-type">>, Req) of
    {<<"application/vision-probe-guest-info-v0">>, Req1} ->
      {ok, Body, Req2} = cowboy_req:body(Req1),
      {{Ip, _Port}, Req3} = cowboy_req:peer(Req2),
      {guest_info, Ip, erlang:binary_to_term(Body), Req3}
  end.

