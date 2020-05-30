-module(anon_sup).
-behaviour(supervisor).
-export([start_link/0,
         start_child/2,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
	io:format("\n-- Anoner app started"),
    supervisor:start_link({local,?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

init([]) ->
    Element = {anon_profile, {anon_profile, start_link, []},
               temporary, brutal_kill, worker, [anon_profile]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

