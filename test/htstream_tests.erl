-module(htstream_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%%
http_request_test() ->
   Req = [<<"GET / HTTP/1.1\r\n">>],
   {_, Http} = decode_request(Req),
   ?assert(header =:= htstream:state(Http)),
   ?assert({'GET', <<"/">>} =:= htstream:request(Http)),
   ?assert([] =:= htstream:headers(Http)),
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
   {{_,_,_}, Http} = decode_request(Req),
   ?assert(eof =:= htstream:state(Http)),
   ?assert([{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}] =:= htstream:headers(Http)),
   ?assert(length(Req) =:= htstream:packets(Http)),
   ?assert(iolist_size(Req) =:= htstream:octets(Http)).

%%
%%
http_post_headers_test() ->
   Req = [
      <<"POST / HTTP/1.1\r\n">>
     ,<<"Host: localhost:80\r\n">>
     ,<<"Accept: */*\r\n">>
     ,<<"\r\n">>
   ],
   {{_,_,_}, Http} = decode_request(Req),
   error_logger:error_report(""),
   ?assert(eoh =:= htstream:state(Http)),
   ?assert([{'Host', <<"localhost:80">>}, {'Accept', [{'*','*'}]}] =:= htstream:headers(Http)),
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