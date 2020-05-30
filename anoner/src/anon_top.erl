-module(anon_top).
-behaviour(supervisor).
-export([init/1,
		 start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
	io:format("\n-- Top supervisor started"),
    Pid = supervisor:start_link({local,?SERVER}, ?MODULE, []),
	{ok,Mods} = application:get_env(modules),
	io:format("\nActive modules:~p\n",[Mods]),
	Pid.

init([]) ->
	{ok,{{one_for_one, 5, 60},
		[{store_pidbarn,{store_pidbarn, start_link,[]},permanent,1000, worker,[store_pidbarn]},
		 {store_sup,{store_sup,start_link,[]},permanent, 5000, supervisor,[store_sup]},
		 {anon_sup, {anon_sup, start_link,[]},permanent, 5000, supervisor,[anon_sup]}
		]}}.

