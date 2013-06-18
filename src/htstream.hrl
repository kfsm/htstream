

-record(http, {
   is       = undefined :: atom(),   %% parser state
   version  = undefined :: any(),
   htline   = undefined :: any(),    %% http request / response
   length   = 0         :: integer(), 
   headers  = []        :: [{atom() | binary(), any()}]
}).
