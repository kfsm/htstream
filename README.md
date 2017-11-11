# htstream 

HTTP request/responce parser for Erlang

## Inspiration

The library implements a simple HTTP stream parser, is designed to handle up/down stream of HTTP protocol. The library is provide a minimal overhead, which make is suitable for high throughtput applcations. The library targets to minimize its external dependencies.


## Changelog

* 1.0.x - release a unified HTTP request/response parsing with symmetic state machine. Both HTTP request/response follows a same parsing logic. 
* 0.12.x - improve http parsing using symmetic state machine.
* 0.11.x - initial production quality release candidate