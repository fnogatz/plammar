:- module(ast, [
    tree/3,
    tree/4
  ]).

:- op(1200, xfx, ==>).

pp(A) :-
  print_term(A, [indent_arguments(2)]).


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
/*
op(P, Specifier, Ops, In, [op(Atom_Tree)|Rest]) :-
  atom(In, [Atom_Tree|Rest]),
  atom_chars(Atom, In),
  member(op(P, Specifier, Atom), Ops).
*/



parse(Prec, Ops, AST, In, Out) :-
  phrase(term(Token_Tree), In, Out),
  Token_Tree = term(Tokens),
  phrase(term(Prec, Ops, AST), Tokens, []).



%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(Tree, In, Rest) :-
  token_(token_(Tree), In, Rest),
  Some_More_Elements = [_|_], % at least one element
  \+((
    token_(_, In, Shorter_Rest),
    append(Some_More_Elements, Shorter_Rest, Rest)
  )).

:- use_module(library(edcgs_expand)).

%% op `*` to denote any number of occurences
:- op(800, fy, *).
*(A, B, C, D) :-
  % only if input list is given 
  \+var(C), !,
  % use `**` to consume as most as possible at first
  sequence('**', A, B, C, D).
*(_A, B, C, D) :-
  % only if input list should be calculated
  var(C),
  C = D,
  B = [].

%% op `?` to denote zero or one occurences
:- op(800, fy, ?).
?(A, B, C, D) :-
  % only if input list is given
%  \+var(C), !,
  sequence('?', A, B, C, D).
/*
?(_A, B, C, D) :-
  % only if input list should be calculated
  var(C),
  C = D,
  B = [].
*/

% :- load_files('iso-grammar.pl', [module(ast)]).
:- load_files('lexer.pl', [module(ast)]).
:- load_files('parser.pl', [module(ast)]).
