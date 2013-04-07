%% Reuse at will.

%% @private
-module(demo).

%% API
-export([message/1]).
-export([number/1]).
-export([hangup/0]).
-export([start/0]).

-spec message(binary()) -> ok.
message(Data) when is_binary(Data) ->
	demo_publisher:message(Data).

-spec number(integer()) -> ok.
number(Value) when is_integer(Value) ->
	demo_publisher:number(Value).

-spec hangup() -> ok.
hangup() ->
	demo_publisher:hangup().

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(eventsource),
	ok = application:start(demo).
