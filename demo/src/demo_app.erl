%% Reuse at will.

%% @private
-module(demo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/eventsource", demo_handler, []},
			{"/", cowboy_static, [
				{directory, {priv_dir, demo, []}},
				{file, <<"index.html">>},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	demo_sup:start_link().

stop(_State) ->
	ok.
