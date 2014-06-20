-module(htstream_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%%
http_request_test() ->
   Req = [<<"GET / HTTP/1.1\r\n">>],
   {_, Http} = decode_request(Req),
   ?assert(header =:= htstream:state(Http)),
   ?assert({request, {'GET', <<"/">>, []}} =:= htstream:http(Http)),
   ?assert(length(Req) =:= htstream:packets(Http)),
   ?assert(iolist_size(Req) =:= htstream:octets(Http)).

%%
%%
http_get_headers_test() ->
   Req = [
      <<"GET / HTTP/1.1\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Accept: */*\r\n">>
     ,<<"\r\n">>
   ],
   Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}],
   {{'GET', <<"/">>, Head}, Http} = decode_request(Req),
   ?assert(eof =:= htstream:state(Http)),
   ?assert({request, {'GET', <<"/">>, Head}} =:= htstream:http(Http)),
   ?assert(length(Req) =:= htstream:packets(Http)),
   ?assert(iolist_size(Req) =:= htstream:octets(Http)).

%%
%%
http_post_headers_test() ->
   Req = [
      <<"POST / HTTP/1.1\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Accept: */*\r\n">>
     ,<<"Content-Length: 10\r\n">>
     ,<<"\r\n">>
   ],
   Head = [{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}, {'Content-Length', 10}],   
   {{'POST', <<"/">>, Head}, Http} = decode_request(Req),
   ?assert(eoh =:= htstream:state(Http)),
   ?assert({request, {'POST', <<"/">>, Head}} =:= htstream:http(Http)),
   ?assert(length(Req) =:= htstream:packets(Http)),
   ?assert(iolist_size(Req) =:= htstream:octets(Http)).

%%
%%
http_websock_test() ->
   Req = [
      <<"GET / HTTP/1.1\r\n">>
     ,<<"Upgrade: websocket\r\n">>
     ,<<"Connection: Upgrade\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Sec-WebSocket-Key: nIbybgjSAkXg7XiX98Zaaw==\r\n">>
     ,<<"Sec-WebSocket-Version: 13\r\n">>
     ,<<"\r\n">>
   ],
   {{'GET', <<"/">>, Head}, Http} = decode_request(Req),
   ?assertMatch(eof,  htstream:state(Http)),
   ?assertMatch({_, <<"nIbybgjSAkXg7XiX98Zaaw==">>}, lists:keyfind(<<"Sec-Websocket-Key">>, 1, Head)),
   ?assertMatch({request, {'GET', <<"/">>, Head}}, htstream:http(Http)),
   ?assertEqual(length(Req), htstream:packets(Http)),
   ?assertEqual(iolist_size(Req), htstream:octets(Http)).



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