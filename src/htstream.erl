%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%   Copyright 2012 Mario Cardona, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%  @description
%%     http stream parser    
-module(htstream).
-include("htstream.hrl").

-export([
   new/0, parse/2,
   eof/1, headers/1, version/1, 
   method/1, url/1,
   status/1, message/1
]).

-type(http() :: #http{}).
-export_type([http/0]).

%%
%% create new http parser
-spec(new/0 :: () -> #http{}).

new()  ->
   #http{is=idle}.

%%
%% check end of http message
-spec(eof/1 :: (#http{}) -> true | false).

eof(#http{is=eof}) ->
   true;
eof(_) ->
   false.

%%
%% return http headers
-spec(headers/1 :: (#http{}) -> list()).

headers(#http{headers=Headers}) ->
   Headers.

%%
%% return http version
-spec(version/1 :: (#http{}) -> {integer(), integer()}).

version(#http{version=Version}) ->
   Version. 

%%
%% return http request method
-spec(method/1 :: (#http{}) -> atom()).

method(#http{method=Mthd}) ->
   Mthd.

%%
%% return http request url
-spec(url/1 :: (#http{}) -> binary()).

url(#http{url=Url}) ->
   Url.

%%
%% return http response status
-spec(status/1 :: (#http{}) -> integer()).

status(#http{status=Code}) ->
   Code.

%%
%% return http response message
-spec(message/1 :: (#http{}) -> binary()).

message(#http{msg=Msg}) ->
   Msg.


%%
%% parse packet and return result triple {HtData, Packet, Http}
%%   HtData - parsed http content or empty binary
%%   Packet - unparsed suffix of packet
%%   Http   - new parser state  
-spec(parse/2 :: (binary(), #http{}) -> {binary(), binary(), #http{}}).

parse(Pckt, #http{is=idle}=S) ->
   maybe_http(erlang:decode_packet(http_bin, Pckt, []), Pckt, S);

parse(Pckt, #http{is=header}=S) ->
   maybe_header(erlang:decode_packet(httph_bin, Pckt, []), Pckt, S);

parse(Pckt, #http{is=entity, length=0}=S) ->
   {<<>>, Pckt, S#http{is=eof}};
parse(Pckt, #http{is=entity, length=Len}=S)
 when size(Pckt) < Len ->
   {Pckt, <<>>,  S#http{length=Len - size(Pckt)}};
parse(Pckt, #http{is=entity, length=Len}=S) ->
   <<Chunk:Len/binary, Rest/binary>> = Pckt,
   {Chunk, Rest, S#http{is=entity, length=0}};

parse(Pckt, #http{is=chunked, length=0}=S) ->
   maybe_chunk_header(binary:split(Pckt, <<"\r\n">>), Pckt, S);
parse(Pckt, #http{is=chunked, length=Len}=S)
 when size(Pckt) < Len ->
   {Pckt, <<>>,  S#http{length=Len - size(Pckt)}};
parse(Pckt, #http{is=chunked, length=Len}=S) ->
   <<Chunk:Len/binary, $\r, $\n, Rest/binary>> = Pckt,
   {Chunk, Rest, S#http{length=0}}.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%% attempt to parse HTTP request line
maybe_http({more, _}, Pckt, S) ->
   {undefined, Pckt, S};
maybe_http({error, _}, _Pckt, _S) ->
   {error, badarg};
maybe_http({ok, {http_error, _}, _}, _Pckt, _S) ->
   {error, badarg};
maybe_http({ok, {http_request, Mthd, Url, Vsn}, Rest}, _Pckt, S) ->
   parse(Rest, 
      S#http{
         is      = header,
         method  = parse_method(Mthd),
         url     = parse_url(Url),
         version = Vsn   
      }
   );
maybe_http({ok, {http_response, Vsn, Status, Msg}, Rest}, _Pckt, S) ->
   parse(Rest,
      S#http{
         is      = header,
         status  = Status,
         msg     = Msg,
         version = Vsn
      }
   ).

%% attempt to parse HTTP header
maybe_header({more, _}, Pckt, S) ->
   {undefined, Pckt, S};
maybe_header({error, _}, _Pckt, _S) ->
   {error, badarg};
maybe_header({ok, {http_error, _}, _}, _Pckt, _S) ->
   {error, badarg};
maybe_header({ok, http_eoh, Rest}, _Pckt, S) ->
   maybe_payload(Rest, S#http{headers=lists:reverse(S#http.headers)});
maybe_header({ok, {http_header, _I, Head, _R, Val}, Rest}, _Pckt, S) -> 
   parse(Rest, S#http{headers=[parse_header(Head, Val)|S#http.headers]}).

%% parse http supported method
parse_method(Mthd) ->
   % TODO: implement
   Mthd.

%% parse url
parse_url(Url) ->
   % TODO: implement
   Url.

%% parse header value
parse_header('Content-Length', Val) ->
   {'Content-Length', list_to_integer(binary_to_list(Val))};
parse_header(Head, Val) ->
   {Head, Val}.

%% check if payload needs to be received
maybe_payload(Rest, #http{method='GET'}=S) ->
   {undefined, Rest, S#http{is=eof}};
maybe_payload(Rest, #http{method='HEAD'}=S) ->
   {undefined, Rest, S#http{is=eof}};
maybe_payload(Rest, #http{method='DELETE'}=S) ->
   {undefined, Rest, S#http{is=eof}};
maybe_payload(Rest, S) ->
   maybe_payload_entity(lists:keyfind('Content-Length', 1, S#http.headers), Rest, S).


maybe_payload_entity({'Content-Length', Len}, Rest, S) ->
   parse(Rest, S#http{is=entity, length=Len});
maybe_payload_entity(false, Rest, S) ->
   maybe_payload_chunked(lists:keyfind('Transfer-Encoding', 1, S#http.headers), Rest, S).


maybe_payload_chunked({'Transfer-Encoding', <<"chunked">>}, Rest, S) ->
   parse(Rest, S#http{is=chunked}).


%% parse chunk header
maybe_chunk_header([_], Pckt, S) ->
   {<<>>, Pckt, S};
maybe_chunk_header([Head, Pckt], _Pckt, S) ->
   [Len |_] = binary:split(Head, [<<" ">>, <<";">>]),
   case list_to_integer(binary_to_list(Len), 16) of
      0   ->
         <<_:2/binary, Rest/binary>> = Pckt,
         {<<>>, Rest, S#http{is=eof}}; 
      Val ->
         parse(Pckt, S#http{length=Val})
   end.

