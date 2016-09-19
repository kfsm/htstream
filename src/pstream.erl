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
%%   packet stream encoder / decoder 
-module(pstream).

-export([
   new/1
  ,packets/1
  ,octets/1
  ,buffer/1
  ,decode/2
  ,encode/2 
]).

%%
%% internal state
-record(stream, {
   type    = undefined :: raw | line | chunk %% type of packet stream
  ,packets = 0         :: integer()  %% number of communicated packets
  ,octets  = 0         :: integer()  %% size of communicated octets
  ,length  = 0         :: integer()  %% length of queue data
  ,recbuf  = undefined :: any()      %% internal receive buffer
}).

%%
%% public types
-type(stream()    :: #stream{}).     %% packet parser state


%%
%% create new stream
-spec new(atom()) -> stream().

new(Type) ->
   #stream{
      type   = Type
     ,recbuf = queue:new()
   }.

%%
%% return number of transmitted octets
-spec octets(stream()) -> integer().

octets(#stream{octets=X}) ->
   X.

%%
%% return number of transmitted packets
-spec packets(stream()) -> integer().

packets(#stream{packets=X}) ->
   X.

%%
%% return buffered stream
-spec buffer(#stream{}) -> binary().

buffer(#stream{recbuf=X}) ->
   X.

%%
%% encode message to stream
-spec encode(binary(), stream()) -> {iolist(), stream()}.

encode(Msg, #stream{type=raw}=State) ->
   {[Msg], 
      State#stream{
         packets = State#stream.packets + 1
        ,octets  = State#stream.octets  + erlang:iolist_size(Msg)
      }
   };

encode(Msg, #stream{type=line}=State) ->
   Pckt = binary:split(Msg, [<<$\r, $\n>>, <<$\n>>]),
   {[<<X/binary, $\n>> || X <- Pckt], 
      State#stream{
         packets = State#stream.packets + length(Pckt)
        ,octets  = State#stream.octets  + erlang:iolist_size(Msg)
      }
   };

encode(Msg, #stream{type=chunk}=State)
 when is_binary(Msg) ->
   Size = list_to_binary(integer_to_list(size(Msg), 16)),
   {[<<Size/binary, $\r, $\n, Msg/binary, $\r, $\n>>],
      State#stream{
         packets = State#stream.packets + 1
        ,octets  = State#stream.octets  + size(Msg)
      }
   }.

%%
%% decode message from stream
-spec decode(binary(), stream()) -> {iolist(), stream()}.

decode(Pckt, #stream{}=State) ->
   decode(Pckt, [], State).

decode(<<>>, Acc, State) ->
   {lists:reverse(Acc), State};
decode(undefined, Acc, State) ->
   {lists:reverse(Acc), State};
decode(Pckt, Acc, #stream{type=raw}=State) ->
   {lists:reverse([Pckt | Acc]), 
      State#stream{
         packets = State#stream.packets + 1
        ,octets  = State#stream.octets  + erlang:iolist_size(Pckt)
      }
   };

decode(Pckt, Acc, #stream{type=line}=State) ->
   case binary:split(Pckt, [<<$\r, $\n>>, <<$\n>>]) of
      %% incoming packet do not have CRLF
      [_] ->
         decode(undefined, Acc,
            State#stream{
               length = State#stream.length + erlang:iolist_size(Pckt)
              ,recbuf = queue:in(Pckt, State#stream.recbuf) 
            }
         );
      %% incoming packet has CRLF
      [Head, Tail] ->
         Msg = erlang:iolist_to_binary(
            queue:to_list(queue:in(Head, State#stream.recbuf))
         ),
         decode(binary:copy(Tail), [binary:copy(Msg) | Acc], 
            State#stream{
               packets = State#stream.packets + 1
              ,octets  = State#stream.octets  + erlang:iolist_size(Msg)
              ,length  = 0
              ,recbuf  = queue:new()
            }
         )
   end;

decode(Pckt, Acc, #stream{type=chunk}=State) ->
   case binary:split(Pckt, [<<$\r, $\n>>]) of
      [_] ->
         decode(undefined, Acc,
            State#stream{
               length = State#stream.length + erlang:iolist_size(Pckt)
              ,recbuf = queue:in(Pckt, State#stream.recbuf) 
            }
         );
      [Head, Tail] ->
         Msg = erlang:iolist_to_binary(
            queue:to_list(queue:in(Head, State#stream.recbuf))
         ),
         Size = list_to_integer(binary_to_list(Msg), 16),
         decode(Tail, Acc, 
            State#stream{
               type    = {chunk, Size}
              ,length  = 0
              ,recbuf  = queue:new()
            }
         )
   end;

decode(Pckt, Acc, #stream{type={chunk, Size}, length=Len}=State)
 when Len + size(Pckt) < Size + 2 ->
   decode(undefined, Acc,
      State#stream{
         length = State#stream.length + erlang:iolist_size(Pckt)
        ,recbuf = queue:in(Pckt, State#stream.recbuf) 
      }
   );

decode(Pckt, Acc, #stream{type={chunk, Size}}=State) ->
   <<Head:Size/binary, $\r, $\n, Tail/binary>> = erlang:iolist_to_binary(
      queue:to_list(queue:in(Pckt, State#stream.recbuf))
   ),
   decode(binary:copy(Tail), [binary:copy(Head) | Acc], 
      State#stream{
         type    = chunk
        ,packets = State#stream.packets + 1
        ,octets  = State#stream.octets  + erlang:iolist_size(Head)
        ,length  = 0
        ,recbuf  = queue:new()
      }
   ).



