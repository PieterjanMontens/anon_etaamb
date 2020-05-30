%
% Anon word score operation.
% Op1: boolean naive sylable succession

-module(anon_op1).

-export([
	db_req/1,
	score_for/1,
	add_term/1,
	del_term/1,
	store_backup/1
	]).

-include("records.hrl").
-include("config.hrl").

-define(DB_PREFIX,"naivebool").
-define(SUBPAT_MASK,"(([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,}[zrtpqsdfghjklmwxcvbn]{1,}$|([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,}[zrtpqsdfghjklmwxcvbn]{1,}(?=[zrtpqsdfghjklmwxcvbn])|([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,})").
-record(rec, { syl,
				 count   = 0,
				 atbegin = 0,
				 atend   = 0,
				 chain   = [] }).

db_req(L) ->
	db_handle(L).

store_backup(Lang) ->
	store:backup(db_handle(Lang)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ADD STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_term(W) ->
	#word{language = Lang, term = RawTerm} = W,
	Term = string:to_lower(RawTerm),
	DB	 = db_handle(Lang),
	Syls = extract(Term),
	add_syls(DB, Syls),
	ok.

add_syls(DB, [Syl|_] = List ) ->
	#rec{atbegin = Atbegin} = Rec = syl_get(DB,Syl),
	syl_set(DB, Syl, Rec#rec {atbegin = Atbegin + 1}),
	add_syls(DB, List, recurse).

	add_syls(_DB, [], recurse) -> ok;
	add_syls(DB, [Syl], recurse) ->
		#rec{count = Count, atend = Atend} = Rec = syl_get(DB, Syl),
		syl_set(DB, Syl, Rec#rec{count = Count+1, atend = Atend+1}),
		ok;

	add_syls(DB, [Syl,Nsyl|Tail], recurse) ->
		#rec{count = Count} = Rec = syl_get(DB, Syl),
		Chain = chain_add(Rec#rec.chain, Nsyl),
		syl_set(DB, Syl, Rec#rec{count = Count+1, chain = Chain}),
		add_syls(DB, [Nsyl|Tail], recurse).

chain_add(Chain,Term) ->
	case proplists:get_value(Term, Chain, false) of
		false -> [{Term,1}|Chain];
		Val	  -> [{Term, Val + 1} | proplists:delete(Term, Chain)]
	end.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEL STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_term(_W) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SCORE STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
score_for(W) ->
	#word{language = Language, term = RawTerm} = W,
	Term = string:to_lower(RawTerm),
	DB	 = db_handle(Language),
	Syls = extract(Term),
	PatternList = [syl_get(DB,Syl) || Syl <- Syls],
	score_make(PatternList).

score_make(Pattern) ->
	Sc = lists:flatten([sc_bool(calc_begin, Pattern)
					  ,sc_bool(calc, Pattern)
					  ,sc_bool(calc_end, Pattern)]),
	io:format("Scores:~p ",[Sc]),
	Sum = anon_stat:sum(Sc),
	io:format("Scores:~p Sum:~p~n",[Sc,Sum]),
	anon_stat:floor(Sum/length(Sc) * 100).


sc_bool(calc_begin, [_Sel|_]) ->
	1;

sc_bool(calc_end, Pattern) ->
	[Sel|_] = lists:reverse(Pattern),
	#rec{count= Count, atend=Atend} = Sel,
	case {Count,Atend} of
		{0,_} -> 0;
		{_,0} -> 0;
		_	  -> 1 
	end;

sc_bool(calc,[Sel,NSel]) ->
	#rec{chain= Chain, count= _Count} = Sel,
	#rec{syl= NSyl} = NSel,
	NScore = case subsyl_count(NSyl,Chain)  of
		0 -> 0;
		_ -> 1 end,
	[NScore];

sc_bool(calc,[Sel,NSel|List]) ->
	[sc_bool(calc,[Sel,NSel]) | sc_bool(calc,[NSel|List])];
sc_bool(calc,[_Sel]) -> [];
sc_bool(calc,_) -> [0].


subsyl_count(Syl,Chain) ->
	case proplists:get_value(Syl, Chain, false) of
		false -> 0;
		Val   -> Val
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTIL STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract(Term) ->
	Pattern = patternize_regex(Term),
	case {length(lists:flatten(Pattern)),length(Term)} of
		{V,V} -> Pattern;
		{A,B} -> io:format("Pattern error.~n Term:~p  Length:~p~n Syls:~p  Length~p~n",[Term,B,Pattern,A]),
				 [Term]
	end.
		
patternize_regex(Term) ->
	Positions = case re:run(Term,?SUBPAT_MASK,[global]) of
					{match,M} -> M;
					nomatch   -> [] end,
	get_subs(Term,Positions).

get_subs(_,[]) -> [];
get_subs(Term,[[{Pos,Len}|_]|T]) ->
	[string:substr(Term,Pos+1,Len) | get_subs(Term,T)].
	
db_handle(Lang) ->
	Str = lists:concat([?DB_PREFIX,"_",Lang]),
    list_to_binary(Str).
	%try
	%	erlang:list_to_existing_atom(Str)
	%catch
	%	error:badarg -> 
	%		erlang:list_to_atom(Str)
	%end.


syl_set(DB, Syl, Sylel) ->
	store:set(DB, Syl, Sylel).

syl_get(DB, Syl) ->
	case store:get(DB, Syl) of
		{ok, Val} -> Val;
		{_,not_found} -> #rec{syl = Syl}
	end.
	
