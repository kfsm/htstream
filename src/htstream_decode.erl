%%
%% @doc
%%   decode binary stream to http messages
-module(htstream_decode).
-include("htstream.hrl").
-include("include/htstream.hrl").

-export([
   htline/2,
   header/2,
   chunk/2
]).


%%
%%
htline(Stream, Http) ->
   codec_htline(erlang:decode_packet(http_bin, Stream, []), Stream, Http).

codec_htline({more, _}, Stream, Http) ->
   {undefined, undefined, 
      Http#http{recbuf = Stream}
   };

codec_htline({error, _}, _Stream, _Http) ->
   exit(badarg);

codec_htline({ok, {http_error, _}, _}, _Stream, _Http) ->
   exit(badarg);

codec_htline({ok, {http_request, Mthd, Url, Vsn}, Stream}, _, Http) ->
   {undefined, Stream, 
      Http#http{
         is      = header,
         type    = request,
         htline  = {codec_method(Mthd), codec_url(Url)},
         version = Vsn   
      }
   };

codec_htline({ok, {http_response, Vsn, Status, Msg}, Stream}, _, Http) ->
   {undefined, Stream,
      Http#http{
         is      = header,
         type    = response,
         htline  = {Status, Msg},
         version = Vsn
      }
   }.

%%
codec_method(Mthd) when is_atom(Mthd) -> Mthd;
codec_method(Mthd) when is_binary(Mthd) -> htstream_codec:a(Mthd).

%% attempt to decode url
codec_url({abs_path, Url}) ->  
   Url;
codec_url({absoluteURI, Scheme, Host, undefined, Path}) ->
   <<(htstream_codec:s(Scheme))/binary, $:, $/, $/, 
      Host/binary, Path/binary>>;
codec_url({absoluteURI, Scheme, Host, Port, Path}) ->
   <<(htstream_codec:s(Scheme))/binary, $:, $/, $/, 
     Host/binary, $:, (htstream_codec:s(Port))/binary, Path/binary>>;
codec_url({scheme, Host, Port}) ->
   <<$h, $t, $t, $p, $:, $/, $/, Host/binary, $:, Port/binary>>;
codec_url('*') ->
   <<$*>>.


%%
%%
header(Stream, Http) ->
   codec_header(erlang:decode_packet(httph_bin, Stream, []), Stream, Http).

codec_header({more, _}, Stream, Http) ->
   {undefined, undefined, 
      Http#http{recbuf = Stream}
   };

codec_header({error, _}, _Stream, _Http) ->
   exit(badarg);

codec_header({ok, {http_error, _}, _}, _Stream, _Http) ->
   exit(badarg);

codec_header({ok, {http_header, _I, Head, _R, Val}, Stream}, _, #http{headers = Tail} = Http) ->
   {undefined, Stream,
      Http#http{
         headers = [codec_header_value(Head, Val) | Tail]
      }
   };

codec_header({ok, http_eoh, Stream}, _, #http{htline = {Mthd, Url}, headers = Head} = Http) ->
   {{Mthd, Url, lists:reverse(Head)}, Stream, 
      Http#http{is = eoh, headers = lists:reverse(Head)}
   }.


   % {Mthd, Url} = State#http.htline,
   % Head        = lists:reverse(State#http.headers),
   % {{Mthd, Url, Head}, decode_check_payload(State#http{headers=Head, recbuf=Rest})}.

%% parse header value
codec_header_value('Content-Length', Val) ->
   {'Content-Length', htstream_codec:i(Val)};
codec_header_value('Transfer-Length', Val) ->
   {'Transfer-Length', htstream_decode:i(Val)};
% codec_header_value('Content-Type', Val) ->
%    {'Content-Type', decode_mime_type(Val)};
% decode_header_value('Accept', Val) ->
%    {'Accept', [decode_mime_type(X) || X <- binary:split(Val, <<$,>>, [trim, global])]};
codec_header_value(Head, Val) ->
   {htstream_codec:s(Head), htstream_codec:s(Val)}.


%%
%%
chunk(Stream, #http{is = chunk_head} = Http) ->
   codec_chunk_head(binary:split(Stream, <<"\r\n">>), Stream, Http);

chunk(Stream, #http{is = chunk_tail} = Http) ->
   codec_chunk_tail(binary:split(Stream, <<"\r\n">>), Stream, Http).

%% 
codec_chunk_head([_], Stream, Http) ->
   {undefined, undefined,
      Http#http{recbuf = Stream}
   };

codec_chunk_head([Head, Stream], _, Http) ->
   [Len |_] = binary:split(Head, [<<" ">>, <<";">>]),
   case list_to_integer(binary_to_list(Len), 16) of
      0   ->
         % TODO: decoder assumes that 0\r\n\r\n is arrived in single packet
         <<_:2/binary, Tail/binary>> = Stream,
         {undefined, undefined,
            Http#http{is = eof, recbuf = Tail}
         };
      Val ->
         {undefined, Stream,
            Http#http{is = chunk_data, length = Val}
         }
   end.

%% 
codec_chunk_tail([_], Stream, Http) ->
   {undefined, undefined,
      Http#http{recbuf = Stream}
   };

codec_chunk_tail([_, Stream], _, Http) ->
   {undefined, Stream,
      Http#http{is=chunk_head, length=0}
   }.
