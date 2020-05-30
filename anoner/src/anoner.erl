-module(anoner).

-export([add/2,
		 del/2,
		 check/2,
		 multi_check/2,
		 sequence_check/2,
		 multi_filter/2,
		 anonymise/3,
		 backup/1
		 ]).

add(Lang, Word) ->
	anon_profile:word_add(object_get(Lang,Word)).

del(Lang, Word) ->
	anon_profile:word_del(object_get(Lang,Word)).

check(Lang, Word) ->
	case anon_profile:word_check(object_get(Lang,Word)) of
		{ok, Score} -> Score;
		{error, _}	-> 0
	end.

backup(Lang) ->
	anon_profile:module_backup(Lang).

multi_check(Lang, String) ->
	List = string:tokens(String," "),
	RList = [anon_profile:word_check(
			object_get(Lang,anon_tools:utf8clean(W))) || W <- List],
	RString = [io_lib:format("~p ",[X]) || {ok,X} <- RList],
	lists:flatten(RString).

sequence_check(Lang, String) ->
	List = [anon_tools:utf8clean(W) || W <- string:tokens(String," ")],
	{ok,RList} = anon_profile:list_check(object_get(list,Lang,List)),
	RString = [io_lib:format("~p ",[X]) || X <- RList],
	lists:flatten(RString).

multi_filter(_Lang, _List) ->
	"Not ready yet".

anonymise(_Lang, _List, _String) ->
	"Not ready yet".


object_get(Lang,Word) ->
	object_get(word,Lang,Word).
object_get(word,Lang,Word) ->
	Request = anon_profile:prepare(Word, Lang),
	anon_profile:create(Request);

object_get(list,Lang,List) ->
	Request = [anon_profile:prepare(W,Lang) || W <- List],
	anon_profile:create(Request).
