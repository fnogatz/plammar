%% op `*` to denote any number of occurences
:- op(800, fy, *).
*_ -->
  [].
*X --> 
  X,
  *X.

%% op `?` to denote optional occurence
:- op(800, fy, ?).
?_ -->
  [].
?X -->
  X.

%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(In, Out) :-
  token_(In, Out),
  (
    % empty rest list
    Out = []
  ;
    Out = [_One_More_Element|Rests],
    % consuming one element more does not succeed
    \+ token_(In, Rests)
  ).

:- consult('iso-grammar.pl').
