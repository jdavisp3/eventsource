%% Reuse at will.

%% @private
-module(demo_handler).
-behaviour(cowboy_loop_handler).

%% API

-export([send/2]).
-export([quit/1]).

%% loop callbacks

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

%% API

-spec send(pid(), eventsource:message()) -> ok.
send(Pid, Message) ->
	erlang:send(Pid, {send, Message}, []).

-spec quit(pid()) -> ok.
quit(Pid) ->
	erlang:send(Pid, quit, []).

%% loop callbacks

init({_Transport, http}, Req, []) ->
	{ok, Req2} = eventsource:init(Req),
	{LastEventId, Req3} = eventsource:last_event_id(Req2),
	demo_publisher:register_source(LastEventId),
	{loop, Req3, undefined}.

info({send, Msg}, Req, State) ->
	eventsource:send(Msg, Req),
	{loop, Req, State};
info(quit, Req, State) ->
	{ok, Req, State}.

terminate(_, _, _) ->
	ok.
