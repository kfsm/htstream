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
   state/1,  headers/1, version/1, 
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
%% check parser state
-spec(state/1 :: (#http{}) -> idle | message | entity | eof).

state(#http{is=idle})    -> idle;
state(#http{is=header})  -> message;
state(#http{is=entity})  -> entity;
state(#http{is=chunked}) -> entity;
state(#http{is=eof})     -> eof.


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
-spec(parse/2 :: (binary(), #http{}) -> {iolist(), binary(), #http{}}).

parse(Pckt, Http) ->
   parse(Pckt, [], Http).

parse(Pckt, Acc, #http{is=idle}=S) ->
   maybe_http(erlang:decode_packet(http_bin, Pckt, []), Pckt, Acc, S);

parse(Pckt, Acc, #http{is=header}=S) ->
   maybe_header(erlang:decode_packet(httph_bin, Pckt, []), Pckt, Acc, S);

parse(Pckt, Acc, #http{is=entity, length=Len}=S)
 when size(Pckt) < Len ->
   {return([Pckt  | Acc]), <<>>, S#http{length=Len - size(Pckt)}};
parse(Pckt, Acc, #http{is=entity, length=Len}=S) ->
   <<Chunk:Len/binary, Rest/binary>> = Pckt,
   {return([Chunk | Acc]), Rest, S#http{is=eof, length=0}};

parse(Pckt, Acc, #http{is=chunked, length=0}=S) ->
   maybe_chunk_header(binary:split(Pckt, <<"\r\n">>), Pckt, Acc, S);
parse(Pckt, Acc, #http{is=chunked, length=Len}=S)
 when size(Pckt) < Len ->
   {return([Pckt  | Acc]), <<>>, S#http{length=Len - size(Pckt)}};
parse(Pckt, Acc, #http{is=chunked, length=Len}=S) ->
   case Pckt of
      <<Chunk:Len/binary, $\r, $\n>> ->
         {return([Chunk | Acc]), <<>>, S#http{length=0}};
      <<Chunk:Len/binary, $\r, $\n, Rest/binary>> ->
         parse(Rest, [Chunk | Acc], S#http{length=0})
   end.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%% output accumulated result
return(Acc) ->
   lists:reverse(Acc).
   

%% attempt to parse HTTP request line
maybe_http({more, _}, Pckt, Acc, S) ->
   {return(Acc), Pckt, S};
maybe_http({error, _}, _Pckt, _Acc, _S) ->
   {error, badarg};
maybe_http({ok, {http_error, _}, _}, _Pckt, _Acc, _S) ->
   {error, badarg};
maybe_http({ok, {http_request, Mthd, Url, Vsn}, Rest}, _Pckt, Acc, S) ->
   parse(Rest, Acc,
      S#http{
         is      = header,
         method  = parse_method(Mthd),
         url     = parse_url(Url),
         version = Vsn   
      }
   );
maybe_http({ok, {http_response, Vsn, Status, Msg}, Rest}, _Pckt, Acc, S) ->
   parse(Rest, Acc,
      S#http{
         is      = header,
         status  = Status,
         msg     = Msg,
         version = Vsn
      }
   ).

%% attempt to parse HTTP header
maybe_header({more, _}, Pckt, Acc, S) ->
   {return(Acc), Pckt, S};
maybe_header({error, _}, _Pckt, _Acc, _S) ->
   {error, badarg};
maybe_header({ok, {http_error, _}, _}, _Pckt, _Acc, _S) ->
   {error, badarg};
maybe_header({ok, http_eoh, Rest}, _Pckt, Acc, S) ->
   maybe_payload(Rest, Acc, S#http{headers=lists:reverse(S#http.headers)});
maybe_header({ok, {http_header, _I, Head, _R, Val}, Rest}, _Pckt, Acc, S) -> 
   parse(Rest, Acc, S#http{headers=[parse_header(Head, Val)|S#http.headers]}).

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
maybe_payload(Rest, Acc, #http{method='GET'}=S) ->
   {return(Acc), Rest, S#http{is=eof}};
maybe_payload(Rest, Acc, #http{method='HEAD'}=S) ->
   {return(Acc), Rest, S#http{is=eof}};
maybe_payload(Rest, Acc, #http{method='DELETE'}=S) ->
   {return(Acc), Rest, S#http{is=eof}};
maybe_payload(Rest, Acc, S) ->
   maybe_payload_entity(lists:keyfind('Content-Length', 1, S#http.headers), Rest, Acc, S).


maybe_payload_entity({'Content-Length', Len}, Rest, Acc, S) ->
   parse(Rest, Acc, S#http{is=entity, length=Len});
maybe_payload_entity(false, Rest, Acc, S) ->
   maybe_payload_chunked(lists:keyfind('Transfer-Encoding', 1, S#http.headers), Rest, Acc, S).


maybe_payload_chunked({'Transfer-Encoding', <<"chunked">>}, Rest, Acc, S) ->
   parse(Rest, Acc, S#http{is=chunked}).


%% parse chunk header
maybe_chunk_header([_], Pckt, Acc, S) ->
   {return(Acc), Pckt, S};
maybe_chunk_header([Head, Pckt], _Pckt, Acc, S) ->
   [Len |_] = binary:split(Head, [<<" ">>, <<";">>]),
   case list_to_integer(binary_to_list(Len), 16) of
      0   ->
         <<_:2/binary, Rest/binary>> = Pckt,
         {return(Acc), Rest, S#http{is=eof}}; 
      Val ->
         parse(Pckt, Acc, S#http{length=Val})
   end.
