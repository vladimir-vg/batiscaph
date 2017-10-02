-module(erlmodules_handler).

% -export([init/3]).
% -export([handle/2]).
% -export([terminate/3]).
% 
% 
% 
% init(_Transport, Req, []) ->
%   {ok, Req, no_state}.
% 
% terminate(_Reason, _Req, _State) ->
%   ok.
% 
% 
% 
% handle(Req, State) ->
%   case cowboy_req:method(Req) of
%     {<<"POST">>, Req1} -> handle0(Req1, State);
%     {_, Req1} ->
%       {ok, Req2} = cowboy_req:reply(405, [], <<>>, Req1),
%       {ok, Req2, State}
%   end.
% 
% handle0(Req, State) ->
%   {Bindings, Req1} = cowboy_req:bindings(Req),
%   Id = proplists:get_value(id, Bindings),
%   case gproc:lookup_pids({n, l, {websocket, Id}}) of
%     [] ->
%       {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req1),
%       {ok, Req2, State};
%     [_] ->
%       {ok, Req2} = read_and_send_modules(Id, Req1),
%       {ok, Req3} = cowboy_req:reply(204, [], <<>>, Req2),
%       {ok, Req3, State}
%   end.
% 
% 
% 
% read_and_send_modules(Id, Req) ->
%   case cowboy_req:part(Req) of
%     {done, Req1} -> {ok, Req1};
%     {ok, Headers, Req1} ->
%       {file, <<"modules">>, Filename, _ContentType, _TE} = cow_multipart:form_data(Headers),
%       Name = filename:basename(Filename, <<".erl">>),
%       {ok, Body, Req2} = read_body(Req1, <<>>),
%       gproc:send({n, l, {websocket, Id}}, {store_module, Name, Body}),
%       read_and_send_modules(Id, Req2)
%   end.
% 
% read_body(Req, Acc) ->
%   case cowboy_req:part_body(Req) of
%     {ok, Body, Req1} -> {ok, <<Acc/binary, Body/binary>>, Req1};
%     {more, Body, Req1} -> read_body(Req1, <<Acc/binary, Body/binary>>)
%   end.
