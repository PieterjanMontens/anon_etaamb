
%%%%%% Word record, used by _profile and _store 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(word, {language,
               term,
               score}).  

-record(sylel, { syl,
				 count   = 0,
				 atend   = 0,
				 chain   = [] }).
