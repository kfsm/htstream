

-record(http, {
   is      = undefined :: atom(),   %% parser state
   version = undefined :: any(),
   % request 
   method  = undefined :: atom(),
   url     = undefined :: binary(),
   % response
   status  = undefined :: undefined | integer(),
   msg     = undefined :: undefined | binary(),
   % common
   length  = 0         :: integer(), 
   headers = []        :: [{atom() | binary(), any()}]
}).
