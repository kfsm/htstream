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
-type(method()  :: atom()).                        % request method
-type(url()     :: binary()).                      % request url 
-type(header()  :: {atom() | binary(), binary() | integer}).  % http header
-type(request() :: {method(), url(), [header()]}). % http request
-type(response():: {integer(), binary(), [header()]}). 
-type(payload() :: binary()).                      % http payload
-type(http()    :: #http{}).                       % http parser state

-define(VERSION,   {1, 1}).

%%
%% create new http stream parser
-spec new() -> http().
-spec new(any()) -> http().

new()  ->
   new(?VERSION).

new({_, _}=Vsn) ->
   #http{is=idle, version=Vsn};

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

decode(#http{}=S) ->
   decode(<<>>, S);
decode(Msg) ->
   decode(Msg, new()).

decode(Msg, #http{recbuf = <<>>}=S) ->
   decode(Msg, [], 
      S#http{
         packets = S#http.packets + 1
        ,octets  = S#http.octets  + erlang:iolist_size(Msg)
      });
decode(Msg, S) ->
   decode(iolist_to_binary([S#http.recbuf, Msg]), [], 
      S#http{
         recbuf = <<>>
        ,packets = S#http.packets + 1
        ,octets  = S#http.octets  + erlang:iolist_size(Msg)
      }).

%%
%% encode http stream
%% returns produces http message and new parser state
-spec encode(iolist() | request() | response(), #http{}) -> {binary(), #http{}}.

encode(Msg) ->
   encode(Msg, new()).
encode(Msg, #http{recbuf = <<>>}=S) ->
   encode(Msg, [], S);
encode(Msg, #http{recbuf = IObf}=S) ->
   encode(iolist_to_binary([IObf, Msg]), [], S#http{recbuf = <<>>}).



%%%------------------------------------------------------------------
%%%
%%% decoder
%%%
%%%------------------------------------------------------------------

%% decode http request
decode(Pckt, _Acc, #http{is=idle}=S) ->
   decode_http(erlang:decode_packet(http_bin, Pckt, []), Pckt, S);

%% decode http headers
decode(Pckt, _Acc, #http{is=header}=S) ->
   decode_header(erlang:decode_packet(httph_bin, Pckt, []), Pckt, S);

%% decode entity payload (end-of-header / entity first byte)
decode(Pckt, Acc, #http{is=eoh, length=chunked}=S) ->
   decode(Pckt, Acc, S#http{is=chunk_head, length=0});
decode(Pckt, Acc, #http{is=eoh}=S) ->
   decode(Pckt, Acc, S#http{is=entity});

%% decode entity payload
decode(Pckt, Acc, #http{is=entity, length=Len}=S)
 when is_integer(Len), size(Pckt) < Len ->
   {lists:reverse([Pckt  | Acc]), S#http{length=Len - size(Pckt)}};

decode(Pckt, Acc, #http{is=entity, length=Len}=S)
 when is_integer(Len) ->
   <<Chunk:Len/binary, Rest/binary>> = Pckt,
   {lists:reverse([Chunk | Acc]), S#http{is=eof, length=0, recbuf=Rest}};

decode(Pckt, Acc, #http{is=entity, length=inf}=S) ->
   % message length is determined by closed connection
   {lists:reverse([Pckt  | Acc]), S};

%% decode chunked payload 
decode(Pckt, Acc, #http{is=chunk_head, length=0}=S) ->
   decode_chunk_head(binary:split(Pckt, <<"\r\n">>), Pckt, Acc, S);

decode(Pckt, Acc, #http{is=chunk_data, length=Len}=S)
 when size(Pckt) < Len ->
   {lists:reverse([Pckt  | Acc]), S#http{length=Len - size(Pckt)}};

decode(Pckt, Acc, #http{is=chunk_data, length=Len}=S) ->
   <<Chunk:Len/binary, Rest/binary>> = Pckt,
   decode(Rest, [Chunk | Acc], S#http{is=chunk_tail, length=0});

decode(Pckt, Acc, #http{is=chunk_tail}=S) ->
   decode_chunk_tail(binary:split(Pckt, <<"\r\n">>), Pckt, Acc, S);  

decode(Pckt, Acc, #http{is=eof}=State) ->
   {lists:reverse(Acc), State#http{recbuf=Pckt}}.


%% attempt to parse http request/response line
decode_http({more, _}, Pckt, S) ->
   {[], S#http{recbuf=Pckt}};
decode_http({error, _}, _Pckt, _S) ->
   exit(badarg);
decode_http({ok, {http_error, _}, _}, _Pckt, _S) ->
   exit(badarg);
decode_http({ok, {http_request, Mthd, Url, Vsn}, Rest}, _Pckt, S) ->
   decode(Rest, [],
      S#http{
         is      = header,
         type    = request,
         htline  = {decode_method(Mthd), decode_url(Url)},
         version = Vsn   
      }
   );
decode_http({ok, {http_response, Vsn, Status, Msg}, Rest}, _Pckt, S) ->
   decode(Rest, [],
      S#http{
         is      = header,
         type    = response,
         htline  = {Status, Msg},
         version = Vsn
      }
   ).

%% attempt to decode method
decode_method(<<"CONNECT">>) ->
   'CONNECT';
decode_method(Mthd) ->
   Mthd.

%% attempt to decode url
decode_url({abs_path, Url}) ->  
   Url;
decode_url({absoluteURI, Scheme, Host, undefined, Path}) ->
   <<(atom_to_binary(Scheme, utf8))/binary, $:, $/, $/, 
      Host/binary, Path/binary>>;
decode_url({absoluteURI, Scheme, Host, Port, Path}) ->
   <<(atom_to_binary(Scheme, utf8))/binary, $:, $/, $/, 
     Host/binary, $:, (list_to_binary(integer_to_list(Port)))/binary, Path/binary>>;
decode_url({scheme, Host, Port}) ->
   <<$h, $t, $t, $p, $:, $/, $/, Host/binary, $:, Port/binary>>;
decode_url('*') ->
   <<$*>>.

%% attempt to parse HTTP header
decode_header({more, _}, Pckt, S) ->
   {[], S#http{recbuf=Pckt}};
decode_header({error, _}, _Pckt, _S) ->
   exit(badarg);
decode_header({ok, {http_error, _}, _}, _Pckt, _S) ->
   exit(badarg);
decode_header({ok, {http_header, _I, Head, _R, Val}, Rest}, _Pckt, S) -> 
   decode(Rest, [], S#http{headers=[decode_header_value(Head, Val)|S#http.headers]});
decode_header({ok, http_eoh, Rest}, _Pckt, State) ->
   {Mthd, Url} = State#http.htline,
   Head        = lists:reverse(State#http.headers),
   {{Mthd, Url, Head}, decode_check_payload(State#http{headers=Head, recbuf=Rest})}.


%% parse header value
decode_header_value('Content-Length', Val) ->
   {'Content-Length', list_to_integer(binary_to_list(Val))};
decode_header_value('Transfer-Length', Val) ->
   {'Transfer-Length', list_to_integer(binary_to_list(Val))};
decode_header_value('Content-Type', Val) ->
   {'Content-Type', decode_mime_type(Val)};
decode_header_value('Accept', Val) ->
   {'Accept', [decode_mime_type(X) || X <- binary:split(Val, <<$,>>, [trim, global])]};
decode_header_value(Head, Val) ->
   {Head, Val}.

%% parse chunk header
decode_chunk_head([_], Pckt, Acc, S) ->
   {lists:reverse(Acc), S#http{recbuf=Pckt}};
decode_chunk_head([Head, Pckt], _Pckt, Acc, S) ->
   [Len |_] = binary:split(Head, [<<" ">>, <<";">>]),
   case list_to_integer(binary_to_list(Len), 16) of
      0   ->
         %decode(Pckt, Acc, S#http{is=chunk_tail, length=0});
         % TODO: decoder assumes that 0\r\n\r\n is arrived in single packet
         <<_:2/binary, Rest/binary>> = Pckt,
         {lists:reverse(Acc), S#http{is=eof, recbuf=Rest}}; 
      Val ->
         decode(Pckt, Acc, S#http{is=chunk_data, length=Val}) 
   end.

%% parse chunk tail
decode_chunk_tail([_], Pckt, Acc, S) ->
   {lists:reverse(Acc), S#http{recbuf=Pckt}};
decode_chunk_tail([_, Pckt], _Pckt, Acc, S) ->
   decode(Pckt, Acc, S#http{is=chunk_head, length=0}).


%%
%% decode check message payload
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
decode_check_payload(#http{htline={'GET',  _}}=State) ->
   case lists:keyfind('Connection', 1, State#http.headers) of
      {_, <<"Upgrade">>} ->
         State#http{is=upgrade};
      _ ->
         State#http{is=eof}
   end;
decode_check_payload(#http{htline={'HEAD', _}}=S) ->
   S#http{is=eof};
decode_check_payload(#http{htline={'DELETE', _}}=S) ->
   S#http{is=eof};
decode_check_payload(#http{htline={'CONNECT', _}}=S) ->
   S#http{is=upgrade};
decode_check_payload(S) ->
   element(2, alt(S, [
      fun is_payload_chunked/1,
      fun is_payload_entity/1,
      fun is_payload_eof/1
   ])).

%%
%% TODO: support q-values
decode_mime_type(Val) ->
   [Mime | _QVal] = binary:split(Val, <<$;>>, []),
   case binary:split(Mime, <<$/>>, []) of
      [<<$*>>, <<$*>>] -> {'*',  '*'};
      [Type,   <<$*>>] -> {Type, '*'};
      [<<$*>>,SubType] -> {'*', SubType};
      [Type,  SubType] -> {Type,SubType};
      [Type]           -> {Type, '*'}
   end.


%%%------------------------------------------------------------------
%%%
%%% encoder
%%%
%%%------------------------------------------------------------------

%%
%%
encode(eof, _Acc, #http{is=idle}=S) ->
   {[], S};
encode(Msg, _Acc, #http{is=idle}=S)   ->
   encode_http(erlang:element(1, Msg), Msg, S);

%% encode http headers
encode(Msg,  Acc, #http{is=header}=S) ->
   encode_header(Msg, Acc, S);

%% decode entity payload (end-of-header / entity first byte)
encode([], Acc, #http{is=eoh, length=undefined}=S) ->
   encode_result(Acc, S#http{is = eof});

encode([], Acc, #http{is=eoh, length=chunked}=S) ->
   encode_result(Acc, S#http{is=chunk_data, length=0});
encode([], Acc, #http{is=eoh}=S) ->
   encode_result(Acc, S#http{is=entity});

encode(Msg, Acc, #http{is=eoh, length=chunked}=S) ->
   encode(Msg, Acc, S#http{is=chunk_data, length=0});
encode(Msg, Acc, #http{is=eoh}=S) ->
   encode(Msg, Acc, S#http{is=entity});


%% encode entity payload
encode(eof,  Acc, #http{is=entity}=S) ->
   encode_result(Acc, S#http{is = eof, length = 0});

encode(Pckt, Acc, #http{is=entity, length=Len}=S)
 when is_integer(Len), size(Pckt) < Len ->
   encode_result([Pckt  | Acc], S#http{length = Len - byte_size(Pckt)});

encode(Pckt, Acc, #http{is=entity, length=Len}=S)
 when is_integer(Len) ->
   %% TODO: preserve Rest to sndbuf
   <<Chunk:Len/binary, _Rest/binary>> = Pckt,
   encode_result([Chunk | Acc], S#http{is = eof, length = 0});

encode(Pckt, Acc, #http{is=entity, length=inf}=S) ->
   % message length is determined by closed connection
   encode_result([Pckt  | Acc], S);


%% encode chunked payload 
encode(eof,  Acc, #http{is=chunk_data}=S) ->
   encode_chunk(<<>>, Acc, S#http{is=eof});
encode(<<>>, Acc, #http{is=chunk_data}=S) ->
   encode_chunk(<<>>, Acc, S#http{is=eof});
encode(Pckt, Acc, #http{is=chunk_data}=S) ->
   encode_chunk(Pckt, Acc, S);

encode(_Pckt, Acc, #http{is=eof}=State) ->
   encode_result(Acc, State).

%% 
encode_result(Acc, #http{}=S) ->
   {lists:reverse(Acc), 
      S#http{
         packets = S#http.packets + 1
        ,octets  = S#http.octets  + erlang:iolist_size(Acc)
      }
   }.

%%
encode_http('OPTIONS', Msg, S) -> encode_http_request(Msg, S);
encode_http('GET', Msg, S)     -> encode_http_request(Msg, S);
encode_http('HEAD', Msg, S)    -> encode_http_request(Msg, S);
encode_http('POST', Msg, S)    -> encode_http_request(Msg, S);
encode_http('PUT', Msg, S)     -> encode_http_request(Msg, S);
encode_http('DELETE', Msg, S)  -> encode_http_request(Msg, S);
encode_http('TRACE', Msg, S)   -> encode_http_request(Msg, S);
encode_http('PATCH', Msg, S)   -> encode_http_request(Msg, S);
encode_http('CONNECT', Msg, S) -> encode_http_request(Msg, S);
encode_http(_, Msg, S)         -> encode_http_response(Msg, S).

%%
encode_http_request({Mthd, Url, _}=Msg, S) ->
   Uri  = encode_url(Url),
   Http = iolist_to_binary([atom_to_binary(Mthd, utf8), $ , Uri, $ , encode_version(S#http.version), $\r, $\n]),
   encode(Msg, [Http], S#http{is=header, type=request, htline={Mthd, Uri}});
encode_http_request({Mthd, Url, _, _}=Msg, S) ->
   Uri  = encode_url(Url),
   Http = iolist_to_binary([atom_to_binary(Mthd, utf8), $ , Uri, $ , encode_version(S#http.version), $\r, $\n]),
   encode(Msg, [Http], S#http{is=header, type=request, htline={Mthd, Uri}}).

encode_http_response({Status, _}=Msg, S) ->
   X = {Code, Text} = encode_status(Status),
   Http = iolist_to_binary([encode_version(S#http.version), $ , integer_to_list(Code), $ , Text, $\r, $\n]),
   encode(Msg, [Http], S#http{is=header, type=response, htline=X});   
encode_http_response({Status, _, _}=Msg, S) ->
   X = {Code, Text} = encode_status(Status),
   Http = iolist_to_binary([encode_version(S#http.version), $ , integer_to_list(Code), $ , Text, $\r, $\n]),
   encode(Msg, [Http], S#http{is=header, type=response, htline=X}).

%%
encode_header({_, Headers}, Acc, State)
 when is_list(Headers) ->
   Head = [<<(encode_header_value(X))/binary, "\r\n">> || X <- Headers],
   Http = [iolist_to_binary([Head, $\r, $\n]) | Acc],
   encode_result(Http, encode_check_payload(State#http{headers=Headers}));
encode_header({_, _Url, Headers}, Acc, S)
 when is_list(Headers) ->
   Head = [<<(encode_header_value(X))/binary, "\r\n">> || X <- Headers],
   Http = [iolist_to_binary([Head, $\r, $\n]) | Acc],
   encode_result(Http, encode_check_payload(S#http{headers=Headers}));
encode_header({_, Headers, Payload}, Acc, S)
 when is_list(Headers) ->
   Head = [<<(encode_header_value(X))/binary, "\r\n">> || X <- Headers],
   Http = [iolist_to_binary([Head, $\r, $\n]) | Acc],
   encode_result(Http, encode_check_payload(S#http{headers=Headers, recbuf = Payload}));
encode_header({_, _Url, Headers, Payload}, Acc, S)
 when is_list(Headers) ->
   Head = [<<(encode_header_value(X))/binary, "\r\n">> || X <- Headers],
   Http = [iolist_to_binary([Head, $\r, $\n]) | Acc],
   encode_result(Http, encode_check_payload(S#http{headers=Headers, recbuf = Payload})).

%%
%% encode check message payload
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
% encode_check_payload(#http{htline={'GET',  _}}=S) ->
%    S#http{is=eof};
% encode_check_payload(#http{htline={'HEAD', _}}=S) ->
%    S#http{is=eof};
% encode_check_payload(#http{htline={'DELETE', _}}=S) ->
%    S#http{is=eof};
encode_check_payload(#http{htline={'CONNECT', _}}=S) ->
   S#http{is=upgrade};
encode_check_payload(S) ->
   element(2, alt(S, [
      fun is_payload_chunked/1,
      fun is_payload_entity/1,
      fun is_payload_eof/1
   ])).

% %% check if payload needs to be transmitted
% encode_check_payload(S) ->
%    encode_check_entity(lists:keyfind('Content-Length', 1, S#http.headers), S).

% encode_check_entity({'Content-Length', Len}, S) ->
%    S#http{is=entity, length=Len};
% encode_check_entity(false, S) ->
%    encode_check_chunked(lists:keyfind('Transfer-Encoding', 1, S#http.headers), S).

% encode_check_chunked({'Transfer-Encoding', chunked}, S) ->
%    S#http{is=chunk_data};
% encode_check_chunked({'Transfer-Encoding', <<"chunked">>}, S) ->
%    S#http{is=chunk_data};
% encode_check_chunked(false, S) ->
%    S#http{is=eof}.

encode_chunk(Chunk, Acc0, S) ->
   encode_result(encode_chunk(Chunk, Acc0), S).
   
encode_chunk(Chunk, Acc)
 when is_binary(Chunk) ->
   Size = integer_to_list(size(Chunk), 16),
   Chnk = iolist_to_binary([<<(list_to_binary(Size))/binary, $\r, $\n>>, Chunk, <<$\r, $\n>>]),
   [Chnk | Acc];
   
encode_chunk([Head | Tail], Acc) ->
   encode_chunk(Tail, encode_chunk(Head, Acc));

encode_chunk([], Acc) ->
   Acc.


%%
encode_version({Major, Minor}) ->
   <<$H, $T, $T, $P, $/, (encode_value(Major))/binary, $., (encode_value(Minor))/binary>>.

%%
encode_url(undefined) ->
   <<$/>>;
encode_url(Url)
 when is_binary(Url) ->
   Url.

%%
%%
encode_header_value({'Host', {Host, Port}}) ->
   <<"Host", ": ", (encode_value(Host))/binary, ":", (encode_value(Port))/binary>>;
encode_header_value({'Content-Type', Val}) ->
   <<"Content-Type", ": ", (encode_mime_type(Val))/binary>>;
encode_header_value({'Accept', Val})
 when is_list(Val) ->
   [H | T] = [encode_mime_type(X) || X <- Val],
   V = [H] ++ [[$,, $ , X] || X <- T],
   <<"Accept", ": ", (iolist_to_binary(V))/binary>>;
encode_header_value({Key, Val}) ->
   <<(encode_value(Key))/binary, ": ", (encode_value(Val))/binary>>.

%%
%%
encode_mime_type({Type, SubType}) ->
   <<(encode_value(Type))/binary, $/, (encode_value(SubType))/binary>>;
encode_mime_type(Val)
 when is_binary(Val) ->
   Val.
  

%%
%% encode header value  
encode_value(Val)
 when is_atom(Val) ->
   atom_to_binary(Val, utf8);

encode_value(Val)
 when is_binary(Val) ->
   Val;

encode_value(Val)
 when is_list(Val) ->
   list_to_binary(Val);

encode_value(Val)
 when is_integer(Val) ->
   list_to_binary(integer_to_list(Val)).



%% encode http status code response
encode_status(100) -> {100, <<"Continue">>};
encode_status(101) -> {101, <<"Switching Protocols">>};
encode_status(200) -> {200, <<"OK">>};
encode_status(201) -> {201, <<"Created">>};
encode_status(202) -> {202, <<"Accepted">>};
encode_status(203) -> {203, <<"Non-Authoritative Information">>};
encode_status(204) -> {204, <<"No Content">>};
encode_status(205) -> {205, <<"Reset Content">>};
encode_status(206) -> {206, <<"Partial Content">>};
encode_status(300) -> {300, <<"Multiple Choices">>};
encode_status(301) -> {301, <<"Moved Permanently">>};
encode_status(302) -> {302, <<"Found">>};
encode_status(303) -> {303, <<"See Other">>};
encode_status(304) -> {304, <<"Not Modified">>};
encode_status(307) -> {307, <<"Temporary Redirect">>};
encode_status(400) -> {400, <<"Bad Request">>};
encode_status(401) -> {401, <<"Unauthorized">>};
encode_status(402) -> {402, <<"Payment Required">>};
encode_status(403) -> {403, <<"Forbidden">>};
encode_status(404) -> {404, <<"Not Found">>};
encode_status(405) -> {405, <<"Method Not Allowed">>};
encode_status(406) -> {406, <<"Not Acceptable">>};
encode_status(407) -> {407, <<"Proxy Authentication Required">>};
encode_status(408) -> {408, <<"Request Timeout">>};
encode_status(409) -> {409, <<"Conflict">>};
encode_status(410) -> {410, <<"Gone">>};
encode_status(411) -> {411, <<"Length Required">>};
encode_status(412) -> {412, <<"Precondition Failed">>};
encode_status(413) -> {413, <<"Request Entity Too Large">>};
encode_status(414) -> {414, <<"Request-URI Too Long">>};
encode_status(415) -> {415, <<"Unsupported Media Type">>};
encode_status(416) -> {416, <<"Requested Range Not Satisfiable">>};
encode_status(417) -> {417, <<"Expectation Failed">>};
encode_status(422) -> {422, <<"Unprocessable Entity">>};
encode_status(500) -> {500, <<"Internal Server Error">>};
encode_status(501) -> {501, <<"Not Implemented">>};
encode_status(502) -> {502, <<"Bad Gateway">>};
encode_status(503) -> {503, <<"Service Unavailable">>};
encode_status(504) -> {504, <<"Gateway Timeout">>};
encode_status(505) -> {505, <<"HTTP Version Not Supported">>};

%encode_status(100) -> <<"100 Continue">>;
%encode_status(101) -> <<"101 Switching Protocols">>;
encode_status(ok)       -> encode_status(200);
encode_status(created)  -> encode_status(201);
encode_status(accepted) -> encode_status(202);
%status(203) -> <<"203 Non-Authoritative Information">>;
encode_status(no_content) -> encode_status(204);
%status(205) -> <<"205 Reset Content">>;
%status(206) -> <<"206 Partial Content">>;
%status(300) -> <<"300 Multiple Choices">>;
%status(301) -> <<"301 Moved Permanently">>;
%status(found) -> <<"302 Found">>;
%status(303) -> <<"303 See Other">>;
%status(304) -> <<"304 Not Modified">>;
%status(307) -> <<"307 Temporary Redirect">>;
encode_status(badarg) -> encode_status(400);
encode_status(unauthorized) -> encode_status(401);
%status(402) -> <<"402 Payment Required">>;
encode_status(forbidden) -> encode_status(403);
encode_status(not_found) -> encode_status(404);
encode_status(enoent)    -> encode_status(404);
encode_status(not_allowed)    -> encode_status(405);
encode_status(not_acceptable) -> encode_status(406);
%status(407) -> <<"407 Proxy Authentication Required">>;
%status(408) -> <<"408 Request Timeout">>;
encode_status(conflict) -> encode_status(409);
encode_status(duplicate)-> encode_status(409);
%status(410) -> <<"410 Gone">>;
%status(411) -> <<"411 Length Required">>;
%status(412) -> <<"412 Precondition Failed">>;
%status(413) -> <<"413 Request Entity Too Large">>;
%status(414) -> <<"414 Request-URI Too Long">>;
encode_status(bad_mime_type) -> encode_status(415);
%status(416) -> <<"416 Requested Range Not Satisfiable">>;
%status(417) -> <<"417 Expectation Failed">>;
%status(422) -> <<"422 Unprocessable Entity">>;
encode_status(not_implemented) -> encode_status(501);
%status(502) -> <<"502 Bad Gateway">>;
encode_status(not_available) -> encode_status(503);
%status(504) -> <<"504 Gateway Timeout">>;
%status(505) -> <<"505 HTTP Version Not Supported">>.
encode_status(_) -> encode_status(500).


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

is_payload_chunked(S) ->
   case lists:keyfind('Transfer-Encoding', 1, S#http.headers) of
      {'Transfer-Encoding', <<"identity">>} ->
         false;
      {'Transfer-Encoding', <<"chunked">>}  ->
         % chunked packed filter shall be applied anyway 
         %
         % case lists:keyfind('Connection', 1, S#http.headers) of
         %    {'Connection', <<"close">>} ->
         %       false;
         %    _ ->
         %       {ok, S#http{is=eoh, length=chunked}}
         % end;
         {ok, S#http{is=eoh, length=chunked}};
      _ ->
         false
   end.

is_payload_entity(S) ->
   case lists:keyfind('Transfer-Length', 1, S#http.headers) of
      {'Transfer-Length', Len} ->
         {ok, S#http{is=eoh, length=Len}};
      _ ->
         case lists:keyfind('Content-Length', 1, S#http.headers) of
            {'Content-Length', Len} ->
               {ok, S#http{is=eoh, length=Len}};
            _ ->
               false
         end
   end. 

is_payload_eof(S) ->
   case lists:keyfind('Connection', 1, S#http.headers) of
      {'Connection', <<"close">>} ->
         {ok, S#http{is=eoh, length=inf}};
      _ ->
         % look like http request / response go not carry on any payload
         {ok, S#http{is=eoh}}
   end.
