%% Reuse at will.

%% @private
-module(demo_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{demo_publisher, {demo_publisher, start_link, []},
		permanent, 5000, worker, [demo_publisher]}],
	{ok, {{one_for_one, 10, 10}, Procs}}.
