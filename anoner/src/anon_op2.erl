-module(anon_op2).

-export([
	db_req/1,
	score_for/1,
	add_term/1,
	del_term/1,
	store_backup/1
	]).

-include("records.hrl").
-include("config.hrl").

-define(DB_PREFIX,"wordbool").

store_backup(Lang) ->
	store:backup(db_handle(Lang)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_term(W) ->
	#word{language = Lang, term = Term} = W,
	DB	  = db_handle(Lang),
	Count = word_get(DB,Term),
	word_set(DB, Term, Count +1),
	ok.

del_term(_W) ->
	ok.

score_for(W) ->
	#word{language = Lang, term = Term} = W,
	DB	  = db_handle(Lang),
	case word_get(DB,Term) of
		0 -> 0;
		_ -> 100
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DB INTERACTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
word_set(DB, Word, Count) ->
	store:set(DB, Word, Count).

word_get(DB, Word) ->
	case store:get(DB, Word) of
		{ok, Count} -> Count;
		{_,not_found} -> 0
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTIL STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db_req(L) -> 
	db_handle(L).

db_handle(Lang) ->
	Str = lists:concat([?DB_PREFIX,"_",Lang]),
	try
		erlang:list_to_existing_atom(Str)
	catch
		error:badarg -> 
			erlang:list_to_atom(Str)
	end.



