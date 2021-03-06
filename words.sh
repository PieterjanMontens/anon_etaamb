#!/usr/bin/env escript
%%! -smp enable -sname script@localhost
-mode(compile).
-define(ANONER,'anoner@localhost').


main(Argv) -> main(Argv,0).

main(["add",Lang]=Argv,C) ->
	try
		case io:get_line('') of
			eof 	  -> halt(1);
			{error,R} -> io:format("Error: ~p~n",[R]),
						 halt(1);
			Line -> 	 Word = lib:nonl(Line),
						 case C rem 1000 of 
						 	0 -> io:format("Word ~p: ~p~n",[C+1,Word]); 
							_ -> ok end,
						 spawn(fun() -> 
							 %CleanWord = utf8tolatin1(Word),
							 CleanWord = Word,
							 rpc:cast(?ANONER,anoner,add, [list_to_atom(Lang), CleanWord])
							 end),
						 main(Argv,C+1)
		end
	catch
		_:Why -> io:format("Error:~p ~n",[Why]), halt(1)
	end;

main(["del",_Lang],_C) ->
	ok;

main(["backup",Lang],_C) ->
	rpc:call(?ANONER,anoner,backup,[list_to_atom(Lang)]),
	ok;

main(["check",Lang]=Argv,_C) ->
	try
		case io:get_line('') of
			eof 	  -> halt(1);
			{error,R} -> io:format("Error: ~p~n",[R]),
						 halt(1);
			Line -> 	 Word = lib:nonl(Line),
						 spawn(fun() -> 
							 %CleanWord = utf8tolatin1(Word),
							 CleanWord = Word,
							 Rep = rpc:call(?ANONER,anoner,check, 
							 				[list_to_atom(Lang), CleanWord]),
							io:format("Received response:~p~n",[Rep])
							 end),
						 main(Argv,0)
		end
	catch
		_:Why -> io:format("Error:~p ~n",[Why]), halt(1)
	end;


main(_,_) ->
	usage().

%old_normalize(Str) ->
%    Accents = [ {"a","[àáâãäå]"}, {"c","ç"}, {"n","ñ"}, 
%                {"ae","æ"}, {"oe","œ"}, {"e","[èéêë]"}, 
%                {"i","[ìíîï]"}, {"o","[òóôõö]"}, {"u","[ùúûü]"}, {"y","[ýÿ]"},
%                {"","[^a-z0-9\._]"}],
%    string:to_lower(lists:foldl(fun({R,F},S) ->
%                    String = unicode:characters_to_binary(S,utf8,latin1),
%					Chain = unicode:characters_to_binary(F,utf8,latin1),
%                    {ok,MP} = re:compile(Chain,[unicode,caseless]),
%                    Res = re:replace(String,MP,R,[{return,list},global]),
%                    Res end,
%                Str,
%                Accents)).
%
%utf8tolatin1(Str) ->
%	io_lib:format("~ts",[unicode:characters_to_binary(Str,utf8,latin1)]).


usage() ->
	io:format("
Anoner word add script
Words are added thru STDIN

words [add|del|check] [french|dutch]

Usage:
	add     : add words to profile
	del     : remove words from profile
	check   : check a word's score

	french  : french language
	dutch   : dutch language

Recommendation:
	Anoner OTP app has to be launched

"),
	halt(1).
