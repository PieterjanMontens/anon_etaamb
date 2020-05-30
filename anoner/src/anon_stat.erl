-module(anon_stat).

-export([mean/1
		,median/1
		,mode/1
		,quantiles/2
		,sum/1
		]).


mean(V) ->
	Count = length(V),
	sum(V) / Count.

mode(V) ->
	Scores = lists:foldl(fun(X,A) ->
		case proplists:get_value(X,A,false) of
			false -> [{X,1}|A];
			Y	  -> [{X,Y+1}|proplists:delete(X,A)]
		end end, [], V),
	{Term, _} = lists:foldl(fun({T,S},{CT,CS}) ->
		if S > CS -> {T,S};
		   true	  -> {CT,CS}
		end end, {none,0},Scores),
	Term.
	
median(V) ->
	[Median] = quantiles(V,2),
	Median.

quantiles(V,Q) ->
	List   = lists:sort(V),
	Length = length(V),
	Factor = Length / Q,
	quantiles_get(List, Q, Factor).


	quantiles_get(L, Q, F) ->
		quantiles_get(L, Q, 1, 1, F, []).
		
	quantiles_get(_, Qtot, Qtot, _, _, Acc) -> lists:reverse(Acc);
	quantiles_get([H|T], Qtot, Qcur, Pos, Factor, Acc) ->
		F = round(math:ceil(Factor * Qcur)),
		if F =:= Pos ->  
			%io:format("~p = ~p, Q:~p/~p, #~p=~p, F:~p, A:~p~n",
			%		   [F,Pos,Qcur,Qtot,Pos,H,Factor,Acc]),
			quantiles_get(T, Qtot, Qcur+1, Pos+1, Factor, [H|Acc]);
		   true 	 ->
			%io:format("~p != ~p, Q:~p/~p, #~p=~p, F:~p, A:~p~n",
			%		   [F,Pos,Qcur,Qtot,Pos,H,Factor,Acc]),
			quantiles_get(T, Qtot, Qcur, Pos+1, Factor, Acc)
		end.
		   	
sum(V) ->
 	lists:foldl(fun(X, Sum) -> X + Sum end
			   ,0
			   ,V).

% Functions provided by auto-imported bifs
% ceil(V) ->
% 	case round(V) of
% 		N when N > V  -> N;
% 		N when N < V  -> N + 1;
% 		N		 	  -> N
% 	end.
% 
% floor(V) ->
% 	case round(V) of
% 		N when N > V  -> N - 1;
% 		N when N < V  -> N;
% 		N		 	  -> N
% 	end.
