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
%%   websocket frame stream encoder / decoder 
-module(wsstream).

-export([
   new/1
  ,state/1
  ,packets/1
  ,octets/1
  ,buffer/1
  ,decode/2
  ,encode/2 
]).

%% internal state
-record(websock, {
   type     = undefined :: client | server               %% stream type 
  ,is       = undefined :: atom()                        %% parser state
  ,code     = undefined :: integer()                     %% frame op-code
  ,mask     = undefined :: binary()                      %% frame mask 
  ,length   = 0         :: atom() | integer()            %% length of expected content
  ,packets  = 0         :: integer()                     %% number of processed packets
  ,octets   = 0         :: integer()                     %% number of processed bytes
  ,recbuf   = <<>>      :: binary()                      %% internal receive buffer
}).

%%
%% public types 
-type(control()    :: atom()).
-type(websock()    :: #websock{}).                       % websocket parser state


%%
%% create new websocket stream parser
-spec new(atom()) -> websock().

new(Type) ->
   #websock{type=Type}.


%%
%% check parser state
%%   * payload - handling payload
%%   * eof     - end of file  
-spec state(websock()) -> payload | eof.

state(#websock{code=8})  -> eof;
state(#websock{})        -> payload.

%%
%% return number of processed packets
-spec packets(websock()) -> integer().

packets(#websock{packets=X}) ->
   X.

%%
%% return number of processed octets
-spec octets(#websock{}) -> integer().

octets(#websock{octets=X}) ->
   X.

%%
%% return buffered stream
-spec buffer(#websock{}) -> binary().

buffer(#websock{recbuf=X}) ->
   X.

%%%------------------------------------------------------------------
%%%
%%% decoder
%%%
%%%------------------------------------------------------------------

%%
%% decodes websocket stream 
%% returns parsed value and new parser state
-spec decode(binary(), websock()) -> {iolist() | control(), websock()}.

decode(Msg, #websock{recbuf = <<>>}=State) ->
   decode(Msg, [], 
      State#websock{
         packets = State#websock.packets + 1
        ,octets  = State#websock.octets  + erlang:iolist_size(Msg)
      });
decode(Msg, State) ->
   decode(iolist_to_binary([State#websock.recbuf, Msg]), [], 
      State#websock{
         recbuf = <<>>
        ,packets = State#websock.packets + 1
        ,octets  = State#websock.octets  + erlang:iolist_size(Msg)
      }).

%%
%% decode frame header
decode(<<_:4, Code:4, Mask:1, Len:7, Rest/binary>>, Acc, #websock{length=0}=State) ->
   decode_length(Rest, Acc, State#websock{is=header, length=Len, code=Code, mask=Mask});

decode(Pckt, Acc, #websock{length=0}=State) ->
   {lists:reverse(Acc), State#websock{recbuf=Pckt}};

decode(Pckt, Acc, #websock{is=header, length=127}=State) ->
   decode_length(Pckt, Acc, State);

decode(Pckt, Acc, #websock{is=header, length=126}=State) ->
   decode_length(Pckt, Acc, State);

decode(Pckt, Acc, #websock{mask=1}=State) ->
   decode_mask(Pckt, Acc, State);

%%
%% decode payload frame
decode(Pckt, Acc, #websock{length=Len}=State)
 when size(Pckt) >= Len ->
   <<Chunk:Len/binary, Rest/binary>> = Pckt,
   decode(Rest, [unmask(State#websock.mask, Chunk) | Acc], State#websock{length=0});

decode(Pckt, Acc, #websock{}=State) ->
   %% @todo: replace bin append with queue and head pattern match
   {lists:reverse(Acc), State#websock{recbuf=Pckt}}.

%%
%% 
decode_length(<<Len:16, Rest/binary>>, Acc, #websock{length=126}=State) ->
   decode_mask(Rest, Acc, State#websock{is=packet, length=Len});

decode_length(Pckt, Acc, #websock{length=126}=State) ->
   {lists:reverse(Acc), State#websock{recbuf=Pckt}};

decode_length(<<Len:64, Rest/binary>>, Acc, #websock{length=127}=State) ->
   decode_mask(Rest, Acc, State#websock{is=packet, length=Len});

decode_length(Pckt, Acc, #websock{length=127}=State) ->
   {lists:reverse(Acc), State#websock{recbuf=Pckt}};

decode_length(Pckt, Acc, State) ->
   decode_mask(Pckt, Acc, State).

%%
%%
decode_mask(<<Mask:4/binary, Rest/binary>>, Acc, #websock{mask=1}=State) ->
   decode(Rest, Acc, State#websock{is=packet, mask=Mask});

decode_mask(Pckt, Acc, #websock{mask=1}=State) ->
   {lists:reverse(Acc), State#websock{recbuf=Pckt}};

decode_mask(Pckt, Acc, State) ->
   decode(Pckt, Acc, State#websock{is=packet}).



%%%------------------------------------------------------------------
%%%
%%% encoder
%%%
%%%------------------------------------------------------------------

%%
%% encode websocket stream
%% returns produces http message and new parser state
-spec encode(iolist() | control(), websock()) -> {binary(), websock()}.

encode(Msg, #websock{}=State)
 when is_list(Msg) ->
   encode(erlang:iolist_to_binary(Msg), [], State);
encode(Msg, #websock{}=State) ->
   encode(Msg, [], State).

encode(eof, Acc, #websock{}=State) ->
   %%        FIN,      EOF  NOMASK  LEN
   Frame = <<1:1, 0:3, 8:4,  0:1,   0:7>>,
   encode_result([Frame | Acc], State#websock{code=8});

encode(Msg, Acc, #websock{type=server}=State)
 when byte_size(Msg) > 16#ffff ->
   %%        FIN,      TEXT  NOMASK  LEN
   Frame = <<1:1, 0:3, 1:4,   0:1,  127:7, (byte_size(Msg)):64, Msg/binary>>, 
   encode_result([Frame | Acc], State);

encode(Msg, Acc, #websock{type=server}=State)
 when byte_size(Msg) > 16#7d ->
   %%        FIN,      TEXT  NOMASK  LEN
   Frame = <<1:1, 0:3, 1:4,   0:1,  126:7, (byte_size(Msg)):16, Msg/binary>>, 
   encode_result([Frame | Acc], State);

encode(Msg, Acc, #websock{type=server}=State) ->
   %%        FIN,      TEXT  NOMASK  LEN
   Frame = <<1:1, 0:3, 1:4,   0:1,  (byte_size(Msg)):7, Msg/binary>>, 
   encode_result([Frame | Acc], State).

%% 
encode_result(Acc, #websock{}=State) ->
   {lists:reverse(Acc), 
      State#websock{
         packets = State#websock.packets + 1
        ,octets  = State#websock.octets  + erlang:iolist_size(Acc)
      }
   }.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%% unmask websocket frame
unmask(0,    Pckt) ->
   Pckt;
unmask(Mask, Pckt) ->
   unmask(0, Mask, Pckt, <<>>).

unmask(I, Mask, <<X:8, Rest/binary>>, Acc) ->
   unmask(I + 1, Mask, Rest, <<Acc/binary, (X bxor binary:at(Mask, I rem 4)):8>>);
unmask(_, _Mask, <<>>, Acc) ->
   Acc.


