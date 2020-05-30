-module(store).

-export([set/3,
		 add/3,
		 del/2,
		 get/2,
		 init/1,
		 backup/1]).

add(Dbase, Key, Value) ->
	set(Dbase, Key, Value).

set(Dbase, Key, Value) ->
	store_dbase:add(Dbase, Key, Value).

del(Dbase, Key) ->
	store_dbase:del(Dbase, Key).

get(Dbase, Key) ->
	store_dbase:get(Dbase, Key).

backup(Dbase) ->
	store_dbase:backup(Dbase).

init(Dbase) ->
	store_dbase:create(Dbase).


