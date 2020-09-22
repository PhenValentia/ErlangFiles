-module(a2).
-export([punct/1,wsp/1,get_word/1,drop_word/1,drop_wsp/1,words/1,get_line/2,drop_line/2,lines/1,check/1]).
-export([text/0,show_line/1,print_line/1,show_doc/1,print_doc/1,test/0]).
-export([replicate/2,show_line_right/1,print_line_right/1,show_doc_right/1,print_doc_right/1,test_right/0]).

%% Is a character punctuation?

-spec punct(integer()) -> boolean().

punct(Ch) -> lists:member(Ch,"\'\"\.\,\ \;\:\!\?\`\(\)").

%% Is a character whitespace?

-spec wsp(integer()) -> boolean().

wsp(Ch) -> lists:member(Ch,"\ \t\n").

%% get a word from the front of the string
%% split at whitespace, as defined in wsp.
%% e.g.
%% a2:get_word("hello? there") = "hello?"

-spec get_word(string()) -> string().

get_word([]) -> [];
get_word([X]) -> [X];
get_word([X|Xs]) ->
    case wsp(X) of
      true -> [];
      false -> [X|get_word(Xs)]
    end.

%% drop a word from the front of the string
%% split at whitespace, as defined in wsp.
%% e.g.
%% a2:drop_word("hello? there") = " there"

-spec drop_word(string()) -> string().

drop_word([]) -> [];
drop_word([X]) -> [X];
drop_word([X|Xs]) ->
    case wsp(X) of
      true -> Xs;
      false -> drop_word(Xs)
    end.

%% drop whitespace from the front of the string
%% e.g.
%% a2:drop_wsp(" there") = "there"

-spec drop_wsp(string()) -> string().

drop_wsp([]) -> [];
drop_wsp([X]) -> [X];
drop_wsp([X|Xs]) ->
    case wsp(X) of
      true -> drop_wsp(Xs);
      false -> [X|Xs]
    end.
	
%% Break a string into words, using the functions above.
%%
%% Assumption: words is always called on a string
%% without white space at the start.
%% e.g. a2:words("hello? there") = ["hello?","there"]

-spec words(string()) -> list(string()).

words([]) -> [];
words([X]) ->
case wsp(X) of
  true -> [[X]];
  false -> []
end;
words([X|Xs]) ->
  [get_word([X|Xs])|words(drop_wsp(drop_word([X|Xs])))].


%% Splitting a list of words into lines, each of
%% which is a list of words.

%% To build a line, take as many words as possible, keeping
%% the total length (including a single inter-word space
%% between adjacent words) below the specified length, which
%% is the second parameter of get_line and drop_line.

%% e.g.
%% 1> a2:get_line(["When", "riding", "at", "night,"],20).
%% ["When","riding","at"]
%% 2> a2:drop_line(["When", "riding", "at", "night,"],20).
%% ["night,"]

-define(LINELEN,40).

-spec get_line(list(string()),integer()) -> list(string()).

   get_line([], _) -> [];
   get_line([X|Xs], Y) ->
     case (Y > 0) and (Y + 1 - string:length(get_word([X])) > 0) of
       true -> [X|get_line(Xs, Y - 1 - string:length(get_word([X])))];
       false -> []
     end.
	 
%% Partner function of get_line: drops a line word of words.

-spec drop_line(string(),integer()) -> list(string()).

  drop_line([], _) -> [];
  drop_line([X|Xs], Y) ->
    case (Y > 0) and (Y + 1 - string:length(get_word([X])) > 0) of
      true -> drop_line(Xs, Y - 1 - string:length(get_word([X])));
      false -> [X|Xs]
    end.

%% Repeatedly apply get_line and drop_line to turn
%% a list of words into a list of lines i.e.
%% a list of list of words.

-spec lines(list(string())) -> list(list(string())).

lines([]) -> [];
lines([X|Xs]) ->
	[get_line([X|Xs], ?LINELEN) | lines(drop_line([X|Xs], ?LINELEN))].

%% Checking that all words no longer than ?LINELEN.

-spec check(list(string())) -> boolean().

check([]) -> true;
check([X|Xs]) ->
	case (string:length(X) > ?LINELEN) of 
		true -> false;
		false -> check(Xs)
	end.

%% Showing and printing lines and documents.

%% Join words, interspersed with spaces, and newline at the end.

-spec show_line(list(string())) -> string().

show_line([W]) -> W ++ "\n";
show_line([W|Ws]) -> W ++ " " ++ show_line(Ws).

%% As for show_line, but padded with spaces at the start to make
%% the length of the line equal to ?LISTLEN.
%% May use the replicate function.

-spec show_line_right(list(string())) -> string().

show_line_right([X]) -> X ++ "\n";
show_line_right([X|Xs]) -> 
	replicate(?LINELEN - (string:length([X|Xs]) + length([X|Xs]) - 1), " ") ++ X ++ " " ++ show_line(Xs).

%% Build a list out of replicated copies of an item.
%% e.g. replicate(5,3) = [3,3,3,3,3].

-spec replicate(integer(),T) -> list(T).

replicate(X,Y) ->
	case X > 0 of
		true -> [Y| replicate(X-1,Y)];
		false -> []
	end.

%% Print out a line, i.e. resolve the layout.

-spec print_line(list(string())) -> ok.

print_line(X) -> io:format(show_line(X)).

%% As for print-line, but right-aligned.

-spec print_line_right(list(string())) -> ok.

print_line_right(X)-> io:format(show_line_right(X)).

%% Show a whole doc, i.e. list of lines.

-spec show_doc(list(list(string()))) -> string().

show_doc(Ls) ->
    lists:concat(lists:map(fun show_line/1, Ls)).

%% Show a whole doc, i.e. list of lines, right aligned.

-spec show_doc_right(list(list(string()))) -> string().

show_doc_right(Ls) -> 
	lists:concat(lists:map(fun show_line_right/1, Ls)).

%% Print a doc.

-spec print_doc(list(list(string()))) -> ok.

print_doc(X) -> io:format(show_doc(X)).

%% Print a doc, right-aligned.

-spec print_doc_right(list(list(string()))) -> ok.

print_doc_right(X) -> io:format(show_doc_right(X)).

%% Test cases

text() -> "When riding at night, make sure your headlight beam is aiming slightly " ++
          "downwards so oncoming traffic can " ++
          "see you without being dazzled. Be sure not to dip it too much, " ++
          "though, as youâ€™ll still want to see the road around 20 metres ahead of you if riding at speed.".

test() -> print_doc(lines(words(text()))).

test_right() -> print_doc_right(lines(words(text()))).

%% Expected outputs

%% 1> a2:test().
%% When riding at night, make sure your
%% headlight beam is aiming slightly
%% downwards so oncoming traffic can see
%% you without being dazzled. Be sure not
%% to dip it too much, though, as youâ€™ll
%% still want to see the road around 20
%% metres ahead of you if riding at speed.
%% ok

%% 2> a2:test_right().
%%     When riding at night, make sure your
%%        headlight beam is aiming slightly
%%    downwards so oncoming traffic can see
%%   you without being dazzled. Be sure not
%%    to dip it too much, though, as youâ€™ll
%%     still want to see the road around 20
%%  metres ahead of you if riding at speed.
%% ok
