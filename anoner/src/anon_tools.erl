-module(anon_tools).

-export([normalize/1,
	     utf8tolatin1/1,
		 utf8clean/1]).

utf8clean(Str) ->
	normalize(utf8tolatin1(Str)).

normalize(Str) ->
    Accents = [ {"a","[àáâãäå]"}, {"c","ç"}, {"n","ñ"}, 
                {"ae","æ"}, {"e","[èéêë]"}, 
                {"i","[ìíîï]"}, {"o","[òóôõö]"}, {"u","[ùúûü]"}, {"y","[ýÿ]"},
                {"","[^a-z0-9\._]"}],
	Compiled = [ {L,re:compile(utf8tolatin1(C),[caseless])} || {L,C} <- Accents],
    lists:foldl(fun({R,{ok,F}},S) ->
                    Res = re:replace(S,F,R,[{return,list},global]),
					%io:format("Checking ~p, String is ~p, Result is ~p~n",[R,S,Res]),
                    Res end,
                Str,
                Compiled).

utf8tolatin1(Str) ->
	%io:format("Compiling ~p. Result:~ts~n",[Str,unicode:characters_to_binary(Str,utf8,latin1)]),
	io_lib:format("~ts",[unicode:characters_to_binary(Str,utf8,latin1)]).


