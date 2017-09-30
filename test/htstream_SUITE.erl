-module(htstream_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("../src/htstream.hrl").
-include_lib("htstream/include/htstream.hrl").

-export([all/0]).
-export([
   encode_request/1,
   decode_request/1,
   decode_request_progressive/1

   % http_request/1,
   % http_get_headers/1,
   % http_post_headers/1,
   % http_websock/1
]).

%%
%%
all() -> 
   [
      encode_request,
      decode_request,
      decode_request_progressive
    % http_request,
    % http_get_headers,
    % http_post_headers,
    % http_websock
   ].


-define(HEAD_SET_A_TERM, 
   [
      {?HTTP_HOST,   <<"localhost:80">>}
     ,{?HTTP_ACCEPT, <<"*/*">>}
   ]
).

-define(HEAD_SET_A_HTTP,
   <<"Host: localhost:80\r\nAccept: */*\r\n\r\n">>
).

-define(SET_A_TERM,
   {'GET', <<"/">>, ?HEAD_SET_A_TERM}
).

-define(SET_A_HTTP,
   <<"GET / HTTP/1.1\r\n">>
).

%%
%%
encode_request(_) ->
   Http1 = htstream:new(),
   {[?SET_A_HTTP, ?HEAD_SET_A_HTTP], Http2} = htstream:encode(?SET_A_TERM, Http1),

   eoh = htstream:state(Http2),
   #http{length = none} = Http2,
   {request, ?SET_A_TERM} = htstream:http(Http2),

   {[], Http3} = htstream:encode(<<>>, Http2),
   eof = htstream:state(Http3).


%%
%%
decode_request(_) ->
   Http1 = htstream:new(),
   Request = erlang:iolist_to_binary([?SET_A_HTTP, ?HEAD_SET_A_HTTP]),
   {[?SET_A_TERM], Http2} = htstream:decode(Request, Http1),

   eoh = htstream:state(Http2),
   #http{length = none} = Http2,
   {request, ?SET_A_TERM} = htstream:http(Http2),

   {[], Http3} = htstream:encode(<<>>, Http2),
   eof = htstream:state(Http3).

%%
%%
decode_request_progressive(_) ->
   Http1 = htstream:new(),
   {_, Http2} = htstream:decode(?SET_A_HTTP, Http1),
   {[?SET_A_TERM], Http3} = htstream:decode(?HEAD_SET_A_HTTP, Http2),

   eoh = htstream:state(Http3),
   #http{length = none} = Http3,
   {request, ?SET_A_TERM} = htstream:http(Http3),

   {[], Http4} = htstream:encode(<<>>, Http3),
   eof = htstream:state(Http4).




% %%
% %%
% http_request(_) ->
%    Req     = [<<"GET / HTTP/1.1\r\n">>],
%    Packets = length(Req),
%    Octets  = iolist_size(Req),

%    {_, Http} = decode_request(Req),
%    header = htstream:state(Http),
%    {request, {<<"GET">>, <<"/">>, []}} = htstream:http(Http),
%    Packets = htstream:packets(Http),
%    Octets = htstream:octets(Http).

% %%
% %%
% http_get_headers(_) ->
%    Req = [
%       <<"GET / HTTP/1.1\r\n">>
%      ,<<"Host: localhost:80\r\n">>
%      ,<<"Accept: */*\r\n">>
%      ,<<"\r\n">>
%    ],
%    Packets = length(Req),
%    Octets  = iolist_size(Req),
   
%    Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}],
%    {{<<"GET">>, <<"/">>, Head}, Http} = decode_request(Req),
%    {request, {<<"GET">>, <<"/">>, Head}} = htstream:http(Http),
%    eof = htstream:state(Http),
%    Packets = htstream:packets(Http),
%    Octets = htstream:octets(Http).

% %%
% %%
% http_post_headers(_) ->
%    Req = [
%       <<"POST / HTTP/1.1\r\n">>
%      ,<<"Host: localhost:80\r\n">>
%      ,<<"Accept: */*\r\n">>
%      ,<<"Content-Length: 10\r\n">>
%      ,<<"\r\n">>
%    ],
%    Packets = length(Req),
%    Octets  = iolist_size(Req),

%    Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}, {'Content-Length', 10}],   
%    {{<<"POST">>, <<"/">>, Head}, Http} = decode_request(Req),
%    eoh = htstream:state(Http),
%    {request, {<<"POST">>, <<"/">>, Head}} = htstream:http(Http),
%    Packets = htstream:packets(Http),
%    Octets = htstream:octets(Http).

% %%
% %%
% http_websock(_) ->
%    Req = [
%       <<"GET / HTTP/1.1\r\n">>
%      ,<<"Upgrade: websocket\r\n">>
%      ,<<"Connection: Upgrade\r\n">>
%      ,<<"Host: localhost:80\r\n">>
%      ,<<"Sec-WebSocket-Key: nIbybgjSAkXg7XiX98Zaaw==\r\n">>
%      ,<<"Sec-WebSocket-Version: 13\r\n">>
%      ,<<"\r\n">>
%    ],
%    Packets = length(Req),
%    Octets  = iolist_size(Req),

%    {{<<"GET">>, <<"/">>, Head}, Http} = decode_request(Req),
%    {request, {<<"GET">>, <<"/">>, Head}} = htstream:http(Http),
%    upgrade = htstream:state(Http),
%    {_, <<"nIbybgjSAkXg7XiX98Zaaw==">>} = lists:keyfind(<<"Sec-Websocket-Key">>, 1, Head),
%    Packets = htstream:packets(Http),
%    Octets = htstream:octets(Http).



%%
%%
encode(Req) ->
   lists:mapfoldl(
      fun(Packet, {_, Http}) -> 
         htstream:encode(Packet, Http)
      end,
      htstream:new(),
      Req
   ).

%%
%%
decode(Req) ->
   lists:mapfoldl(
      fun(Packet, {_, Http}) -> 
         htstream:decode(Packet, Http)
      end,
      htstream:new(),
      Req
   ).