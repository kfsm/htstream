-module(htstream_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%%
http_request_test() ->
   Req = [<<"GET / HTTP/1.1\r\n">>],
   {_, Http} = decode_request(Req),
   ?assert(header =:= htstream:state(Http)),
   ?assert({'GET', <<"/">>, []} =:= htstream:request(Http)),
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
   ?assert({'GET', <<"/">>, Head} =:= htstream:request(Http)),
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
   ?assert({'POST', <<"/">>, Head} =:= htstream:request(Http)),
   ?assert(length(Req) =:= htstream:packets(Http)),
   ?assert(iolist_size(Req) =:= htstream:octets(Http)).



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