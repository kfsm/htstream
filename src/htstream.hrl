
%%
%%
-record(http, {
   is       = undefined :: atom(),                       %% parser state
   type     = undefined :: request | response,           %% stream type 
   version  = undefined :: any(),                        %% http version
   htline   = undefined :: any(),                        %% http request / response line
   length   = undefined :: atom() | integer(),           %% length of expected content
   headers  = []        :: [{atom() | binary(), any()}], %% list of http headers
   packets  = 0         :: integer(),                    %% number of processed packets
   octets   = 0         :: integer(),                    %% number of processed bytes
   recbuf   = <<>>      :: binary()                      %% internal receive buffer
}).
