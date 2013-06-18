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
   %% send http request
   {ok,  Sock} = gen_tcp:connect(Host, 80, [binary, {active, false}]),
   {Req, _, _} = htstream:encode({'GET', <<"/">>, [{'Host', {Host, 80}}]}),
   ok = gen_tcp:send(Sock, Req),

   %% receive http response
   loop(gen_tcp:recv(Sock, 0), Sock, htstream:new(), <<>>, undefined).

%% recv raw packet from socket and apply parser on it
loop({ok, Packet}, Sock, Http, Buffer, Response) ->
   http(htstream:decode(iolist_to_binary([Buffer, Packet]), Http), Sock, Response);
loop(Error, Sock, _Http, _Buffer, _Response) ->
   gen_tcp:close(Sock),
   Error.

%% http parse result is returned
http({{Code, _, Heads}, Buffer, Http}, Sock, _Response) ->
   http(htstream:decode(Buffer, Http), Sock, {Code, Heads, []});

http({Chunk, Buffer, Http}, Sock, {Code, Heads, Acc}) ->
   case htstream:state(Http) of
      % end of http message, return received iolist
      eof  -> 
         gen_tcp:close(Sock),
         {Code, Heads, lists:reverse([Chunk | Acc])};
      % input packet do not contain entire data, receive more data from socket
      _    ->
         loop(gen_tcp:recv(Sock, 0), Sock, Http, Buffer, {Code, Heads, [Chunk | Acc]})
   end.
