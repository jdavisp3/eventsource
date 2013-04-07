%% Reuse at will.

%% @private
-module(demo_publisher).
-behaviour(gen_server).

%% API

-export([start_link/0]).
-export([message/1]).
-export([number/1]).
-export([hangup/0]).
-export([register_source/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	next_id = 0 :: integer(),
	sources = [] :: [{reference(), pid()}]
}).

-define(SERVER, ?MODULE).

%% API.

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec message(binary()) -> ok.
message(Data) when is_binary(Data) ->
	gen_server:call(?SERVER, {message, Data}).

-spec number(integer()) -> ok.
number(Value) when is_integer(Value) ->
	gen_server:call(?SERVER, {number, Value}).

-spec hangup() -> ok.
hangup() ->
	gen_server:call(?SERVER, hangup).

-spec register_source(binary() | undefined) -> ok.
register_source(undefined) ->
	gen_server:call(?SERVER, {register_source, self(), undefined});
register_source(LastEventId) when is_binary(LastEventId) ->
	gen_server:call(?SERVER, {register_source, self(), LastEventId}).

%% gen_server.

%% @private
init([]) ->
	{ok, #state{}}.

%% @private
handle_call({message, Data}, _From, State0) ->
	{Id, State1} = next_id(State0),
	Event = {event, [{id, Id}, {data, Data}]},
	publish(Event, State1#state.sources),
	{reply, ok, State1};
handle_call({number, Value}, _From, State0) ->
	{Id, State1} = next_id(State0),
	Event = {event, [{id, Id}, {type, <<"number">>},
		{dataline, integer_to_list(Value)}]},
	publish(Event, State1#state.sources),
	{reply, ok, State1};
handle_call(hangup, _From, State) ->
	hangup(State#state.sources),
	{reply, ok, State};
handle_call({register_source, Pid, LastEventId}, _From, State0) ->
	error_logger:info_report({new_source, Pid, LastEventId}),
	MRef = erlang:monitor(process, Pid),
	Sources = [{MRef, Pid}|State0#state.sources],
	State1 = State0#state{sources=Sources},
	publish({retry, 5000}, [{MRef, Pid}]),
	publish({comment, <<"eventsource demo">>}, [{MRef, Pid}]),
	{reply, ok, State1};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MRef, process, Pid, _}, State0) ->
	error_logger:info_report({source_closed, Pid}),
	Sources = lists:delete({MRef, Pid}, State0#state.sources),
	State1 = State0#state{sources=Sources},
	{noreply, State1};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

next_id(#state{next_id=Id}=State) ->
	{Id, State#state{next_id=Id+1}}.

publish(Event, [{_, Pid}|Sources]) ->
	demo_handler:send(Pid, Event),
	publish(Event, Sources);
publish(_, []) ->
	ok.

hangup([{_, Pid}|Sources]) ->
	demo_handler:quit(Pid),
	hangup(Sources);
hangup([]) ->
	ok.
