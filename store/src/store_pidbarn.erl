-module(store_pidbarn).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0, add/2, lookup/1, check/1]).
-define(BARN,pidbarn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API & STARTUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	io:format("\n-- PidBarn started"),
	gen_server:start_link({local, ?MODULE},?MODULE, dict:new(), []).

add(Pid,Dbase) ->
	gen_server:cast(?MODULE, {add,{Pid,Dbase}}).


check(Dbase) ->
	case gen_server:call(?MODULE, {check,Dbase}) of
		{ok,R} -> R;
		_	   -> throw(pidbarn_check_error)
	end.
	
lookup(Dbase) -> 
	case gen_server:call(?MODULE, {lookup,Dbase}) of
		{ok, Pid} -> Pid;
		_		  -> io:format("PidBarn error: no such dbase"),
					 throw(no_such_dbase_in_pidbarn)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OTP CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(S) ->
	{ok,S}.

handle_call({lookup,Dbase}, _From, Dict) ->
	try
		[Pid] = dict:fetch(Dbase,Dict),
		{reply,{ok,Pid}, Dict}
	catch
		T:S -> io:format("Catched Exc: ~p:~p",[T,S]),
			   {error, not_found}
	end;

handle_call({check,Dbase}, _From, Dict) ->
	{reply,{ok,dict:is_key(Dbase,Dict)},Dict};	

handle_call(C, _From, S) ->
	io:format("Unknown call ~p\n",[C]),
	{stop, normal, S}.

handle_cast({add,{Pid,Dbase}}, Dict) ->
	{noreply,dict:append(Dbase,Pid,Dict)};
	
handle_cast(C, S) ->
	io:format("Unknown cast ~p\n",[C]),
	{stop, normal, S}.

handle_info(I, S)->
	io:format("Unknown info ~p\n",[I]),
	{ok, S}.

terminate(R,S) ->
	io:format("DB ~p Terminating. Reason: ~p\n",[S,R]),
	ok.

code_change(_OldVsn, S, _Extra) ->
	{ok, S}.


%ensure() ->
%	case ets:info(?BARN) of
%		undefined 	-> init();
%		_			-> ok
%	end.
%
%init() -> 
%	io:format("-- Pid Barn started\n"),
%	ets:new(?BARN, [public, named_table]).
%
%add(Pid,Dbase) ->
%	ets:insert(?BARN, {Dbase, Pid}).
%
%lookup(Dbase) ->
%	case ets:lookup(?BARN,Dbase) of
%		[{Dbase,Pid}] -> {ok,Pid};
%		[]			  -> {error, not_found}
%	end.


