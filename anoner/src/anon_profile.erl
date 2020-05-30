-module(anon_profile).
-behaviour(gen_server).

-export([start_link/2,
		 prepare/2,
         create/2, create/1,
         word_add/1,
         word_del/1,
         word_check/1,
		 list_check/1,
		 module_backup/1
         ]).
        
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("config.hrl").
-include("records.hrl").

-define(SERVER,?MODULE).
-define(DEFAULT_LEASE, 60*60). % One hour life time
-define(SCORE_V,1).

-record(state, {word,
                lease_time, 
                start_time}).

-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API & STARTUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Word, LeaseTime) ->
    gen_server:start_link(?MODULE, [Word, LeaseTime], []).

prepare(Word, Language) ->
    #word{language = Language,
          term     = cleanword(Word),
          score    = undefined}.

create(Word, LeaseTime) when is_record(Word,word) ->
    case anon_sup:start_child(Word,LeaseTime) of
		{ok, Pid} -> Pid;
		_		  -> throw(could_not_create)
	end;

create(List, LeaseTime) when is_list(List) ->
    case anon_sup:start_child(List,LeaseTime) of
		{ok, Pid} -> Pid;
		_		  -> throw(could_not_create)
	end.
	

create(Word) ->
    create(Word,?DEFAULT_LEASE).

word_add(Pid)->
    gen_server:cast(Pid,add).

word_del(Pid)->
    gen_server:cast(Pid,del).

word_check(Pid)->
    gen_server:call(Pid,check).

list_check(Pid) ->
	gen_server:call(Pid,list_check).

module_backup(Lang) ->
	[apply(M,store_backup,[Lang]) || M <- get_env(modules)].
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OTP CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Word, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok,
        #state{word=Word,
               lease_time = LeaseTime,
               start_time = StartTime},
        time_left(StartTime, LeaseTime)}.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.

handle_call(check, _From, State) ->
    #state{ word  = Word,
             lease_time = LeaseTime,
             start_time = StartTime} = State,
    TimeLeft  = time_left(StartTime, LeaseTime),
    Reply = case Word#word.score of
                undefined -> score(Word);
                CachedVal -> CachedVal
            end,
    {reply, {ok, Reply }, State#state{ word = Word#word{score = Reply}}, TimeLeft};

handle_call(list_check, _From, State) ->
	#state{ word = List} = State,
	Reply = [score(Word) || Word <- List],
    {reply, {ok, Reply }, State, 0}.
		

handle_cast(add, State) ->
    #state{ word  = Word} = State,
    add(Word),
    {stop, normal, State};

handle_cast(del, State) ->
    #state{ word  = Word} = State,
    del(Word),
    {stop, normal, State}.

handle_info(timeout, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IMPLEMENTATION STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score(W) ->
	Scores = [apply(M,score_for,[W]) || M <- get_env(modules)],
	round(math:floor(anon_stat:sum(Scores) / length(Scores))).
	%anon_op2:score_for(W).
	%Scores = [anon_op1:score_for(W), anon_op2:score_for(W)],
	%anon_stat:floor(anon_stat:sum(Scores) / length(Scores)).


add(W) ->
	[apply(M,add_term,[W]) || M <- get_env(modules)],
	ok.
 
del(_W) ->
	ok.                                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT TREATMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cleanword(Str) ->
	anon_tools:normalize(anon_tools:utf8tolatin1(Str)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TOOLIES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(Param) ->
	{ok,Value} = application:get_env(anoner,Param),
	Value.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OLD JUNK - DELETE !!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%    #word{language = Language, term = Term} = W,
%	Syls = anon_patterns:extract(Term),
%	PatternList = [pattern_retrieve(Syl,Language) || Syl <- Syls],
%	Score = score_aggregation(PatternList),
%	io:format("~p score: ~p , Syls: ~p~n",[Term,Score, Syls]),
%	Score.
%
%score_aggregation(Pattern) ->
%	Scores = [sc_bool(Pattern)],
%	anon_stat:floor(anon_stat:sum(Scores) / length(Scores)).
%
%
%sc_bool(Pattern) ->
%	Sc = [sc_bool(calc_begin, Pattern)] 
%	  ++ lists:flatten(sc_bool(calc, Pattern))
%	  ++ [sc_bool(calc_end, Pattern)],
%	io:format("Scores:~p ",[Sc]),
%	Sum = anon_stat:sum(Sc),
%	io:format("Scores:~p Sum:~p~n",[Sc,Sum]),
%	anon_stat:floor(Sum/length(Sc) * 100).
%
%
%sc_bool(calc_begin, [_Sel|_]) ->
%	1;
%
%sc_bool(calc_end, Pattern) ->
%	[Sel|_] = lists:reverse(Pattern),
%	#sylel{count= Count, atend=Atend} = Sel,
%	case {Count,Atend} of
%		{0,_} -> 0;
%		{_,0} -> 0;
%		_	  -> 1 
%	end;
%
%sc_bool(calc,[Sel,NSel]) ->
%	#sylel{chain= Chain, count= _Count} = Sel,
%	#sylel{syl= NSyl} = NSel,
%	NScore = case anon_patterns:subsyl_count(NSyl,Chain)  of
%		0 -> 0;
%		_ -> 1 end,
%	[NScore];
%sc_bool(calc,[Sel,NSel|List]) ->
%	[sc_bool(calc,[Sel,NSel]) | sc_bool(calc,[NSel|List])];
%sc_bool(calc,[_Sel]) -> [];
%sc_bool(calc,_) -> [0].


%
%score(2, calc, [Sylel,[NextSylel|_]]) ->
%	#sylel{chain= Chain, count= _Count} = Sylel,
%	#sylel{syl= NextSyl,atend= NextAtend} = NextSylel,
%	NextScore = anon_patterns:subsyl_count(NextSyl,Chain),
%	Vector = anon_patterns:chain_vector(Chain),
%	case length(Vector) of
%		N when N > 12 ->
%					[Q1,Q2,Q3] = anon_stat:quantiles(Vector, 4),
%					if	NextScore =:= 0  -> 0;
%						NextScore < Q1 -> 50;
%						NextScore < Q2 -> 60;
%						NextScore < Q3 -> 80;
%						true		   -> 100
%					end;
%		N when N > 3 ->
%					Mean = anon_stat:mean(Vector),
%					if NextScore > Mean   -> 100;
%					   NextScore =:= 0 -> 0;
%					   true 		   -> 80
%					end;
%		N when N > 0 ->
%					Mode = anon_stat:mode(Vector),
%					if NextScore >= Mode -> 100;
%					   NextScore =:= 0 	 -> 0;
%					   true	 			 -> 80
%					end;
%		0			-> 
%					if NextAtend > 0 -> 100;
%					   true	         -> 0
%					end
%	end;
%					   
%
%	score(2, endcalc, [Sylel]) ->
%		#sylel{count= _Count, atend=Atend, chain=Chain} = Sylel,
%		Vector = anon_patterns:chain_vector(Chain),
%		Sum    = anon_stat:sum(Vector),
%		if Atend > Sum   -> 100;
%		   Atend > Sum/10 -> 80 ;
%		   Atend =:= 0	 -> 0;
%		   true			 -> 70
%		end;
%
%	score(2, reduce, [Scores]) ->
%		Total = lists:foldl(fun(X,S) -> X+S end, 0, Scores),
%		Score = anon_stat:floor(Total/length(Scores)),
%		Score.
	                                       
%pattern_parse(_Lang, [], _Operation) -> ok;
%pattern_parse(Lang,  [Syl|Patterns], Operation) ->
%	SylEl  = pattern_retrieve(Syl,Lang),
%	NewVal = pattern_apply(Operation, SylEl,Patterns),
%	if
%		NewVal#sylel.count =:= 0 ->
%			anon_store:delete(Lang,Syl);	
%		true					 ->
%			anon_store:replace(Lang,Syl,NewVal) end,
%	pattern_parse(Lang, Patterns, Operation).
%
%
%pattern_retrieve(Syl,Lang) ->
%	case anon_store:retrieve(Lang, Syl) of
%		{ok, Val}     -> Val;
%		{_,not_found} -> #sylel{syl = Syl}
%	end.
%
%pattern_apply(add, Sylel, Patterns) ->
%	Count = Sylel#sylel.count + 1,
%	case Patterns of
%		[]			-> Atend = Sylel#sylel.atend + 1,
%					   Chain = Sylel#sylel.chain;
%		[Nextsyl|_] -> Atend = Sylel#sylel.atend,
%					   Chain = chain_add(Sylel#sylel.chain, Nextsyl)
%	end,
%	Sylel#sylel{ count = Count, atend = Atend, chain = Chain};
%					   
%pattern_apply(del, Sylel, Patterns) ->
%	Count = decrease(Sylel#sylel.count),
%	case Patterns of
%		[]			-> Atend = decrease(Sylel#sylel.atend),
%					   Chain = Sylel#sylel.chain;
%		[Nextsyl|_] -> Atend = Sylel#sylel.atend,
%					   Chain = chain_rem(Sylel#sylel.chain, Nextsyl)
%	end,
%	Sylel#sylel{ count = Count, atend = Atend, chain = Chain}.
%
%decrease(0) -> 0;
%decrease(Val) -> Val - 1.
%
%chain_add(Chain,Term) ->
%	case proplists:get_value(Term, Chain, false) of
%		false -> [{Term,1}|Chain];
%		Val	  -> [{Term, Val + 1} | proplists:delete(Term, Chain)]
%	end.
%	
%chain_rem(Chain,Term)->
%	case proplists:get_value(Term, Chain, false) of
%		false -> Chain;
%		1	  -> proplists:delete(Term,Chain);
%		Val	  -> [{Term, Val - 1} | proplists:delete(Term, Chain)]
%	end.


