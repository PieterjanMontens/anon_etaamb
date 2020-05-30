-module(anon_patterns).

-export([extract/1]).
-compile(export_all).


-define(GROUP_LENGTH,2).

extract(Term) ->
	Pattern = patternize_regex(Term),
	case {length(lists:flatten(Pattern)),length(Term)} of
		{V,V} -> Pattern;
		{A,B} -> io:format("Pattern error.~n Term:~p  Length:~p~n Syls:~p  Length~p~n",[Term,B,Pattern,A]),
				 throw(bad_pattern_result)
	end.
		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REGEX PATTERNING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SUBPAT_MASK,"(([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,}[zrtpqsdfghjklmwxcvbn]{1,}$|([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,}[zrtpqsdfghjklmwxcvbn]{1,}(?=[zrtpqsdfghjklmwxcvbn])|([zrtpqsdfghjklmwxcvbn]{1,})?[aeyuio]{1,})").

patternize_regex(Term) ->
	Positions = case re:run(Term,?SUBPAT_MASK,[global]) of
					{match,M} -> M;
					nomatch   -> [] end,
	get_subs(Term,Positions).
	

get_subs(_,[]) -> [];
get_subs(Term,[[{Pos,Len}|_]|T]) ->
	[string:substr(Term,Pos+1,Len) | get_subs(Term,T)].
	

%%%%%%%%%%%%%%%%%%%%%%%%%%% PATTERN SCORE PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subsyl_count(Syl,Chain) ->
	case proplists:get_value(Syl, Chain, false) of
		false -> 0;
		Val   -> Val
	end.

chain_total(Chain) ->
	lists:foldl(fun(X, Sum) -> X + Sum end
			   ,0
			   , [V || {_,V} <- Chain] ).

chain_average(Chain) ->
	case chain_total(Chain) of
		0 -> 0;
		V -> length(Chain) / V
	end.

chain_vector(Chain) ->
	[V || {_,V} <- Chain].

