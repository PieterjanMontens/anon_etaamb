-module(anon_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("config.hrl").

start(_StartType, _StartArgs) ->
	{ok,Mods}  = application:get_env(modules),
	{ok,Langs} = application:get_env(languages),
    case anon_top:start_link() of
        {ok, Pid} -> [store:init(apply(M,db_req,[L])) || M <- Mods, L <- Langs],
					 {ok,Pid};
        Other     -> {error, Other}
    end.

stop(_State) ->
    ok.
