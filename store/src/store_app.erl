-module(store_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	case store_sup:start_link() of
		{ok, Pid} -> {ok, Pid};
		Other	  -> {error, Other}
	end.

stop(State) ->
	io:format("Stopping with state:~p\n",[State]),
	ok.
