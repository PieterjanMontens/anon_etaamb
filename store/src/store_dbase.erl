-module(store_dbase).
-behaviour(gen_server).
        
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1,
		 create/1,
		 add/3,
		 del/2,
		 get/2,
		 backup/1]).
%-export([start_link/2,
%		 add/2,
%		 update/2,
%		 del/1,
%		 retrieve/1
%         ]).

-define(SERVER,?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API & STARTUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Dbase) ->
    gen_server:start_link(?MODULE, Dbase, []).

add(Dbase, Key, Val) ->
	gen_server:cast(getPid(Dbase),{add,{Key,Val}}).

del(Dbase, Key) ->
	gen_server:cast(getPid(Dbase),{del,Key}).

get(Dbase, Key) ->
	gen_server:call(getPid(Dbase),{get,Key}).

backup(Dbase) ->
	gen_server:call(getPid(Dbase),{backup}).

getPid(Dbase) ->
	Check = store_pidbarn:check(Dbase),
	if Check ->
		store_pidbarn:lookup(Dbase);
	   true	->
	   	create(Dbase)
	end.
	
create(Dbase) ->
	case store_sup:start_child(Dbase) of
		{ok, Pid} -> store_pidbarn:add(Pid,Dbase),
					 Pid;
		Err		  -> io:format("Create error:~p\n",[Err]),
					 throw(could_not_create)
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OTP CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Dbase) ->
	case ets:file2tab(file_name(Dbase), [{verify, true}]) of
		{ok,T} -> io:format("DB ~p loaded from ~p~n",[T,file_name(Dbase)]),
				  {ok,Dbase};
		_	   -> Dbase = ets:new(Dbase, [public, named_table]),
				  io:format("DB ~p initialized.\n",[Dbase]),
				  {ok,Dbase}
	end.

handle_call({get,Key}, _From, Dbase) ->
	case ets:lookup(Dbase, Key) of
		[{Key,Value}] -> {reply, {ok,Value}, Dbase};
		[]			  -> {reply, {error,not_found}, Dbase}
	end;

handle_call({backup}, _From, Dbase) ->
	io:format("Backuping ~p to file ~p\n",[Dbase,file_name(Dbase)]),	
	ok = ets:tab2file(Dbase, file_name(Dbase), [{extended_info,[object_count,md5sum]}]),
	{reply, {ok,Dbase}, Dbase};

handle_call(C, _From, S) ->
	io:format("Unknown call ~p\n",[C]),
	{stop, normal, S}.

handle_cast({add,D}, Dbase) ->
	ets:insert(Dbase, D),
	{noreply, Dbase};

handle_cast({del,Key}, Dbase) ->
	ets:delete(Dbase, Key),
	{noreply, Dbase};

handle_cast({backup}, Dbase) ->
	io:format("Backuping ~p to file ~p\n",[Dbase,file_name(Dbase)]),	
	ok = ets:tab2file(Dbase, file_name(Dbase), [{extended_info,[object_count,md5sum]}]),
	{noreply, Dbase};

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_name(Dbase) ->
	lists:concat(["./store/priv/",Dbase,".bckp"]).
