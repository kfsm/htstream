-module(htstream_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   http_request/1,
   http_get_headers/1,
   http_post_headers/1,
   http_websock/1
]).

%%
%%
all() -> 
  [
      http_request,
      http_get_headers,
      http_post_headers,
      http_websock
  ].

%%
%%
http_request(_) ->
   Req     = [<<"GET / HTTP/1.1\r\n">>],
   Packets = length(Req),
   Octets  = iolist_size(Req),

   {_, Http} = decode_request(Req),
   header = htstream:state(Http),
   {request, {<<"GET">>, <<"/">>, []}} = htstream:http(Http),
   Packets = htstream:packets(Http),
   Octets = htstream:octets(Http).

%%
%%
http_get_headers(_) ->
   Req = [
      <<"GET / HTTP/1.1\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Accept: */*\r\n">>
     ,<<"\r\n">>
   ],
   Packets = length(Req),
   Octets  = iolist_size(Req),
   
   Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}],
   {{<<"GET">>, <<"/">>, Head}, Http} = decode_request(Req),
   {request, {<<"GET">>, <<"/">>, Head}} = htstream:http(Http),
   eof = htstream:state(Http),
   Packets = htstream:packets(Http),
   Octets = htstream:octets(Http).

%%
%%
http_post_headers(_) ->
   Req = [
      <<"POST / HTTP/1.1\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Accept: */*\r\n">>
     ,<<"Content-Length: 10\r\n">>
     ,<<"\r\n">>
   ],
   Packets = length(Req),
   Octets  = iolist_size(Req),

   Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}, {'Content-Length', 10}],   
   {{<<"POST">>, <<"/">>, Head}, Http} = decode_request(Req),
   eoh = htstream:state(Http),
   {request, {<<"POST">>, <<"/">>, Head}} = htstream:http(Http),
   Packets = htstream:packets(Http),
   Octets = htstream:octets(Http).

%%
%%
http_websock(_) ->
   Req = [
      <<"GET / HTTP/1.1\r\n">>
     ,<<"Upgrade: websocket\r\n">>
     ,<<"Connection: Upgrade\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Sec-WebSocket-Key: nIbybgjSAkXg7XiX98Zaaw==\r\n">>
     ,<<"Sec-WebSocket-Version: 13\r\n">>
     ,<<"\r\n">>
   ],
   Packets = length(Req),
   Octets  = iolist_size(Req),

   {{<<"GET">>, <<"/">>, Head}, Http} = decode_request(Req),
   {request, {<<"GET">>, <<"/">>, Head}} = htstream:http(Http),
   upgrade = htstream:state(Http),
   {_, <<"nIbybgjSAkXg7XiX98Zaaw==">>} = lists:keyfind(<<"Sec-Websocket-Key">>, 1, Head),
   Packets = htstream:packets(Http),
   Octets = htstream:octets(Http).



%%
%%
decode_request(Req) ->
   lists:foldl(
      fun(X, {_, Http}) -> 
         htstream:decode(X, Http)
      end,
      {undefined, htstream:new()},
      Req
   ).