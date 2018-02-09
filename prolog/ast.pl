:- module(ast, [
    tree/3,
    tree/4
  ]).

:- op(1200, xfx, ==>).

tree(Body, In, Tree) :-
  tree(Body, In, Tree, []).

tree(Body, In, Tree, Rest) :-
  Body =.. BodyList,
  append(BodyList, [Tree], BodyWithResList),
  BodyWithRes =.. BodyWithResList,
  phrase(BodyWithRes, In, Rest).

tree_from_file(Body, Filename, Tree) :-
  read_file_to_codes(Filename, Codes, []),
  maplist(char_code, Chars, Codes),
  tree(Body, Chars, Tree).

/* 6.3.4.3 Operators */

op(P, Specifier, Ops, In, [op(Atom_Tree)|Rest]) :-
  atom(In, [Atom_Tree|Rest]),
  atom_chars(Atom, In),
  member(op(P, Specifier, Atom), Ops).


%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(In, [token(Token_Res)|Rest]) :-
  token_(In, [token(Token_Res)|Rest]),
  (
    % empty rest list
    Rest = []
  ;
    Rest = [_One_More_Element|Rests],
    % consuming one element more does not succeed
    \+ token_(In, [token(_)|Rests])
  ).

:- use_module(library(edcgs_expand)).

%% op `*` to denote any number of occurences
:- op(800, fy, *).
*(A,B,C,D) :- sequence('*', A, B, C, D).

%% op `?` to denote zero or one occurences
:- op(800, fy, ?).
?(A,B,C,D) :- sequence('?', A, B, C, D).

:- consult('iso-grammar.pl').
