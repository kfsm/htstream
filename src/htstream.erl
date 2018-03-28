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
%%     http stream-oriented encoder / decoder.
%%  
%%  @todo
%%     * optimize recbuf binary, append is expensive, make lists / queue model
%%     * limit size recbuf
-module(htstream).
-include("htstream.hrl").

-export([
   new/0
  ,new/1
  ,state/1
  ,version/1
  ,http/1
  ,packets/1
  ,octets/1
  ,buffer/1
  ,decode/1
  ,decode/2
  ,encode/1
  ,encode/2 
]).

-export_type([
   method/0, url/0, header/0, request/0, payload/0, http/0
]).

%%
%% public types 
-type method()  :: atom().                           % request method
-type url()     :: binary().                         % request url 
-type header()  :: {binary(), binary() | integer()}. % http header
-type headers() :: [header()].
-type request() :: {method(), url(), headers()}.     % http request
-type response():: {integer(), binary(), headers()}. % 
-type payload() :: binary().                         % http payload
-type http()    :: #http{}.                          % http parser state

-define(VERSION,   {1, 1}).


%%
%% create new http stream parser
-spec new() -> http().
-spec new(any()) -> http().

new()  ->
   new(?VERSION).

new({_, _}=Vsn) ->
   #http{is=idle, version=Vsn};

%% @deprecated
%%   method is designed to switch stream to new state during i/o
%%   new version of ht stream should switch to idle automatically
new(#http{recbuf=Buf, version=Vsn}) ->
   #http{is=idle, version=Vsn, recbuf=Buf}.

%%
%% check parser state
%%   idle    - 
%%   header  - handling headers
%%   payload - handling payload
%%   eoh     - end of headers
%%   eof     - end of message  
%%   upgrade - upgrade is requested
-spec state(#http{}) -> idle | header | payload | eof | upgrade.

state(#http{is=idle})    -> idle;
state(#http{is=header})  -> header;
state(#http{is=entity})  -> payload;
state(#http{is=chunk_head}) -> payload;
state(#http{is=chunk_data}) -> payload;
state(#http{is=chunk_tail}) -> payload;
state(#http{is=eoh})     -> eoh;
state(#http{is=eof})     -> eof;
state(#http{is=upgrade}) -> upgrade.


%%
%% return version of http stream
-spec version(#http{}) -> {integer(), integer()} | undefined.

version(#http{version=X}) ->
   X.

%%
%% return http request / response
-spec http(#http{}) -> {request | response, any()} | undefined.

http(#http{type=request,  htline={Method, Path}, headers=Head}) ->
   {request,  {Method, Path, Head}};

http(#http{type=response, htline={Status,  Msg}, headers=Head}) ->
   {response, {Status, Msg,  Head}};

http(#http{}) ->
   undefined.

%%
%% return number of processed packets
-spec packets(#http{}) -> integer().

packets(#http{packets=X}) ->
   X.

%%
%% return number of processed octets
-spec octets(#http{}) -> integer().

octets(#http{octets=X}) ->
   X.

%%
%% return buffered stream
-spec buffer(#http{}) -> binary().

buffer(#http{recbuf=X}) ->
   X.

%%
%% decodes http stream 
%% returns parsed value and new parser state
-spec decode(binary(), #http{}) -> {iolist() | request() | response(), #http{}}.

decode(#http{} = Http) -> 
   decode(<<>>, Http);
decode(Stream) ->
   decode(Stream, new()).

decode(Stream, #http{is = eof, version = Vsn, recbuf = IoBuf}) ->
   decode(Stream, #http{is = idle, version = Vsn, recbuf = IoBuf});

decode(Stream, #http{recbuf = RecBuf} = Http) ->
   stream(
      stream_stats(Stream, Http#http{recbuf = undefined}), 
      join(RecBuf, Stream), 
      [], 
      htstream_decode
   ).

stream_stats(undefined, Http) ->
   Http;
stream_stats(Stream, #http{packets = Pack, octets = Byte} = Http) ->
   Http#http{
      packets = Pack + 1
     ,octets  = Byte + erlang:iolist_size(Stream)
   }.

%%
%% encode http stream
%% returns produces http message and new parser state
-spec encode(iolist() | request() | response(), #http{}) -> {binary(), #http{}}.

encode(Stream) ->
   encode(Stream, new()).

encode(Stream, #http{is = eof, version = Vsn, recbuf = IoBuf}) ->
   encode(Stream, #http{is = idle, version = Vsn, recbuf = IoBuf});

encode(Stream, #http{recbuf = RecBuf} = Http) ->
   stream(Http#http{recbuf = undefined}, join(RecBuf, Stream), [], htstream_encode).


join(undefined, Y) ->
   Y;
join(<<>>,      Y) ->
   Y; 
join(X, undefined) ->
   X;
join(X, Y) ->
   erlang:iolist_to_binary([X, Y]).


%%%------------------------------------------------------------------
%%%
%%% stream parser of http protocol
%%%
%%%------------------------------------------------------------------

%%
%% htline
stream(#http{is = idle} = Http, eof, Queue, Codec) ->
   continue(
      {undefined, undefined, Http},
      Queue,
      Codec
   );

stream(#http{is = idle} = Http, Stream, Queue, Codec) ->
   continue(Codec:htline(Stream, Http), Queue, Codec);

%%
%% headers
stream(#http{is = header} = Http, Stream, Queue, Codec) ->
   continue(Codec:header(Stream, Http), Queue, Codec);

stream(#http{is = eoh, length = undefined} = Http, Stream, Queue, Codec) ->
   continue(
      {undefined, undefined, check_payload(Http#http{recbuf = Stream})},
      Queue,
      Codec
   );

%%
%% type and length of entity payload (end-of-header / entity first byte)
stream(#http{is = eoh, length = none} = Http, _Stream, Queue, Codec) ->
   continue(
      {undefined, undefined, Http#http{is = eof}},
      Queue,
      Codec
   );

stream(#http{is = eoh, length = chunked} = Http, Stream, Queue, Codec) ->
   continue(
      {undefined, Stream, Http#http{is = chunk_head, length = 0}},
      Queue,
      Codec
   );

stream(#http{is = eoh} = Http, Stream, Queue, Codec) ->
   continue(
      {undefined, Stream, Http#http{is = entity}},
      Queue,
      Codec
   );

%%
%% entity payload
stream(#http{is = entity, length = inf} = Http, eof, Queue, Codec) ->
   continue(
      {undefined, undefined, Http#http{is = eof}},
      Queue,
      Codec
   );

stream(#http{is = entity, length = inf} = Http, Stream, Queue, Codec) ->
   % message length is determined by duration of the connection
   continue(
      {Stream, undefined, Http},
      Queue,
      Codec
   );

stream(#http{is = entity, length = Len} = Http, Stream, Queue, Codec)
 when is_integer(Len), size(Stream) < Len ->
   continue(
      {Stream, undefined, Http#http{length = Len - size(Stream)}},
      Queue,
      Codec
   );

stream(#http{is = entity, length = Len} = Http, Stream, Queue, Codec)
 when is_integer(Len) ->
   <<Head:Len/binary, Tail/binary>> = Stream,
   continue(
      {Head, undefined, Http#http{is = eof, length = 0, recbuf = Tail}},
      Queue,
      Codec
   );

stream(#http{is = chunk_head, length = 0} = Http, Stream, Queue, Codec) ->
   continue(Codec:chunk(Stream, Http), Queue, Codec);

stream(#http{is = chunk_data, length = Len} = Http, Stream, Queue, Codec)
 when size(Stream) < Len ->
   continue(
      {Stream, undefined, Http#http{length = Len - size(Stream)}},
      Queue,
      Codec
   );

stream(#http{is = chunk_data, length = Len} = Http, Stream, Queue, Codec) ->
   <<Head:Len/binary, Tail/binary>> = Stream,
   continue(
      {Head, Tail, Http#http{is = chunk_tail, length = 0}},
      Queue,
      Codec
   );

stream(#http{is = chunk_tail} = Http, Stream, Queue, Codec) ->
   continue(Codec:chunk(Stream, Http), Queue, Codec).





%%
continue({undefined, undefined, Http}, Queue, _) ->
   {lists:reverse(Queue), Http};

continue({Pckt, undefined, Http}, Queue, _) ->
   {lists:reverse([Pckt | Queue]), Http};

continue({undefined, Stream, Http}, Queue, Codec) ->
   stream(Http, Stream, Queue, Codec);

continue({Pckt, Stream, Http}, Queue, Codec) ->
   stream(Http, Stream, [Pckt | Queue], Codec).


%%
%% check message payload
%% see http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4
%%
%% 1. Any response message which "MUST NOT" include a message-body 
%%    (such as the 1xx, 204, and 304 responses and any response to 
%%    a HEAD request) is always terminated by the first empty line 
%%    after the header fields... (Not implemented)
%%
%% 2. If a Transfer-Encoding header field (section 14.41) is present 
%%    and has any value other than "identity", then the transfer-length 
%%    is defined by use of the "chunked" transfer-coding (section 3.6), 
%%    unless the message is terminated by closing the connection.
%%
%% 3. If a Content-Length header field (section 14.13) is present, 
%%    its decimal value in OCTETs represents both the entity-length 
%%    and the transfer-length. The Content-Length header field MUST NOT 
%%    be sent if these two lengths are different
%%
%% 4. If the message uses the media type "multipart/byteranges"... 
%%    (Not implemented)
%%
%% 5. By the server closing the connection. 
%%
check_payload(#http{htline={'GET',  _}, headers = Head}=State) ->
   case lists:keyfind(<<"Connection">>, 1, Head) of
      {_, <<"Upgrade">>} ->
         State#http{is=upgrade};
      _ ->
         % State#http{is=eof}
         element(2, alt(State, [
            fun is_payload_chunked/1,
            fun is_payload_entity/1,
            fun is_payload_eof/1
         ]))
   end;
% decode_check_payload(#http{htline={?HTTP_HEAD, _}}=State) ->
%    State#http{is=eof};
% decode_check_payload(#http{htline={?HTTP_DELETE, _}}=State) ->
%    State#http{is=eof};
check_payload(#http{htline={'CONNECT', _}}=State) ->
   State#http{is=upgrade};
check_payload(State) ->
   element(2, alt(State, [
      fun is_payload_chunked/1,
      fun is_payload_entity/1,
      fun is_payload_eof/1
   ])).


%%%------------------------------------------------------------------
%%%
%%% utility
%%%
%%%------------------------------------------------------------------

%% alternation: applies sequence of function  
alt(X, HoF) ->
   alt(undefined, HoF, X).
alt({ok, _}=Y, _, _) ->
   Y;
alt(_, [H|T], X) ->
   alt(H(X), T, X);
alt(Y, [], _) ->
   Y.

is_payload_chunked(#http{headers = Head} = State) ->
   case lists:keyfind(<<"Transfer-Encoding">>, 1, Head) of
      {_, <<"identity">>} ->
         false;
      {_, <<"chunked">>}  ->
         {ok, State#http{is=eoh, length=chunked}};
      _ ->
         false
   end.

is_payload_entity(#http{headers = Head} = State) ->
   case lists:keyfind(<<"Transfer-Length">>, 1, Head) of
      {_, Len} ->
         case htstream_codec:i(Len) of
            Val when Val > 0 ->
               {ok, State#http{is = eoh, length = Val}};
            _ ->
               false
         end;
      _ ->
         case lists:keyfind(<<"Content-Length">>, 1, Head) of
            {_, Len} ->
               case htstream_codec:i(Len) of
                  Val when Val > 0 ->
                     {ok, State#http{is = eoh, length = Val}};
                  _ ->
                     false
               end;
            _ ->
               false
         end
   end. 

is_payload_eof(#http{headers = Head} = State) ->
   case lists:keyfind(<<"Connection">>, 1, Head) of
      {_, <<"close">>} ->
         % Note: The "Connection: close" indicates that message is terminated by 
         % the server closing the connection. 
         % This method is not feasible here, it is impossible to detect Request EOF
         % without heuristics and rules. This feature is more close to HTTP/1.0
         % and will not be supported here.
         % Set length = inf to switch it on.
         {ok, State#http{is = eoh, length = none}};
      _ ->
         % look like http request / response go not carry on any payload
         {ok, State#http{is = eoh, length = none}}
   end.

