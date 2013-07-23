

-record(http, {
   is       = undefined :: atom(),   %% parser state
   version  = undefined :: any(),
   htline   = undefined :: any(),    %% http request / response
   length   = undefined :: atom() | integer(), 
   headers  = []        :: [{atom() | binary(), any()}],
   recbuf   = <<>>      :: binary()  %% internal receive buffer
}).
