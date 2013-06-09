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
%%    example htstream parser application.
-module(hturl).
-include("htstream.hrl").

-export([
   get/1
]).

%%
%% get root content from host
-spec(get/1 :: (list()) -> {integer(), list(), iolist()}).

get(Host)
 when is_list(Host) ->
   {ok, Sock} = gen_tcp:connect(Host, 80, [binary, {active, false}]),
   ok = gen_tcp:send(Sock, <<"GET / HTTP/1.1\r\nHost: ", (list_to_binary(Host))/binary, "\r\n\r\n">>),
   loop(gen_tcp:recv(Sock, 0), Sock, htstream:new(), <<>>, []).

%% recv raw packet from socket and apply parser on it
loop({ok, Packet}, Sock, Http, Buffer, Entity) ->
   http(htstream:parse(iolist_to_binary([Buffer, Packet]), Http), Sock, Entity);
loop(Error, _Sock, _Http, _Buffer, _Entity) ->
   Error.

%% http parse result is returned
http({Chunk, Buffer, Http}, Sock, Entity) ->
   case htstream:state(Http) of
      % end of http message, return received iolist
      eof  -> 
         gen_tcp:close(Sock),
         {htstream:status(Http), htstream:headers(Http), lists:reverse([Chunk | Entity])};
      % input packet do not contain entire data, receive more data from socket
      _    ->
         loop(gen_tcp:recv(Sock, 0), Sock, Http, Buffer, [Chunk | Entity])
   end.
