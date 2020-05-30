-module(store_sup).
-behaviour(supervisor).
-export([start_link/0,
	     start_child/1,
		 init/1
		 ]).

-define(SERVER, ?MODULE).

start_link() ->
	io:format("\n-- Store app started\n"),
	supervisor:start_link({local,?SERVER}, ?MODULE, []).

start_child(Dbase) ->
	supervisor:start_child(?SERVER, [Dbase]).

init([]) ->
	Element = {store_dbase, {store_dbase, start_link, []},
               temporary, brutal_kill, worker, [store_dbase]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
	
