%%
%% @doc
%%   encode binary stream to http messages
-module(htstream_encode).
-include("htstream.hrl").

-export([
   htline/2,
   header/2,
   chunk/2
]).


%%
%% http request / response is triple
%%   -type request() :: {method(), url(), headers()}.     
%%   -type response():: {integer(), binary(), headers()}.
%% 
htline({Mthd, Url, _} = Stream, #http{version = Vsn} = Http)
 when is_atom(Mthd)  -> 
   Uri = codec_url(Url),
   {
      iolist_to_binary([htstream_codec:s(Mthd), $ , Uri, $ , codec_version(Vsn), $\r, $\n]), 
      Stream, 
      Http#http{is=header, type=request, htline={Mthd, Uri}}
   };

htline({Code, Text, _} = Stream, #http{version = Vsn} = Http)
 when is_integer(Code) -> 
   {
      htstream_codec:s([codec_version(Vsn), $ , htstream_codec:s(Code), $ , htstream_codec:s(Text), $\r, $\n]),
      Stream,
      Http#http{is = header, type = response, htline = {Code, Text}}
   }.

%%
codec_url(undefined) ->
   <<$/>>;
codec_url(Url) ->
   htstream_codec:s(Url).

%%
codec_version({Major, Minor}) ->
   <<$H, $T, $T, $P, $/, (htstream_codec:s(Major))/binary, $., (htstream_codec:s(Minor))/binary>>.


%%
%%
header({_, _, Head}, #http{} = Http) ->
   {
      htstream_codec:s([[codec_header(X) || X <- Head], $\r, $\n]),
      <<>>,  %% Note: this triggers check payload
      Http#http{is = eoh, headers = Head}
   }.

codec_header({Key, Val}) ->
   <<(htstream_codec:s(Key))/binary, $:, $ , (htstream_codec:s(Val))/binary, $\r, $\n>>.


%%
%%
chunk(eof, Http) ->
   chunk(<<>>, Http#http{is = eof});

chunk(Stream, Http)
 when is_binary(Stream) ->
   Size = htstream_codec:s(integer_to_list(size(Stream), 16)),
   Chnk = iolist_to_binary([Size, $\r, $\n, Stream, $\r, $\n]),
   {Chnk, undefined, Http};

chunk([Stream | Tail], Http)
 when is_binary(Stream) ->
   Size = htstream_codec:s(integer_to_list(size(Stream), 16)),
   Chnk = iolist_to_binary([Size, $\r, $\n, Stream, $\r, $\n]),
   {Chnk, Tail, Http}.
