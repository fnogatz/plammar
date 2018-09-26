:- module(ast, [
    tree/3,
    tree/4,
    parse/2,
    parse/3
  ]).

pp(A) :-
  print_term(A, [indent_arguments(0)]).

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


parse(DCGBody, In) :-
  string_chars(In, Chars),
  parse(DCGBody, Chars, []).

parse(DCGBody, In, Out) :-
  phrase(term(Token_Tree), In, Out),
  Token_Tree = term(Tokens),
  phrase(DCGBody, Tokens, []).


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

/*
  *(DCGBody, Tree, In, Out) <-

  op `*` to denote any number of occurences.
  The distinction depending on the groundness
  of `In` is done only for performing reasons;
  if the input list `In` is given, it is more
  likely that many items can be consumed;
  whereas with an unbound `In` and given `Tree`
  we want to created the smallest possibilities
  at first.
*/
:- op(800, fy, *).
*(DCGBody, Tree, In, Out) :-
  % only if input list is given 
  \+var(In), !,
  % use `**` to consume as most as possible at first
  sequence('**', DCGBody, Tree, In, Out).
*(DCGBody, Tree, In, Out) :-
  % only if input list should be calculated
  var(In), !,
  % use `*` to produce as small as possible at first
  sequence('*', DCGBody, Tree, In, Out).

%% op `?` to denote zero or one occurences
:- op(800, fy, ?).
?(DCGBody, Tree, In, Out) :-
  sequence('?', DCGBody, Tree, In, Out).

:- load_files('lexer.pl', [module(ast)]).
:- load_files('parser.pl', [module(ast)]).
