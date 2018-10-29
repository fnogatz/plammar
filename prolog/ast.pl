:- module(ast, [
    tree/3,
    tree/4,
    parse/2,
    parse/3,
    prolog/3,
    prolog/4
  ]).

:- reexport(iso).

:- use_module(library(clpfd)).

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


prolog(Ops, Tree, In) :-
  string_chars(In, Chars),
  !,
  prolog(Ops, Tree, Chars, []).

prolog(Ops, Tree, In) :-
  prolog(Ops, Tree, In, []).

prolog(Ops, Tree, In, Out) :-
  p_text(Ops, Tree, In, Out).

p_text(_Ops, p_text([]), In, In).

p_text(Ops, p_text([Clause_Tree|Rec]), In, Out) :-
  parse(ast:clause_term(Ops, Clause_Tree), In, Rest),
  p_text(Ops, p_text(Rec), Rest, Out).

p_text(Ops, p_text([Directive_Tree|Rec]), In, Out) :-
  parse(ast:directive_term(Ops, Directive_Tree), In, Rest),
  p_text(Ops, p_text(Rec), Rest, Out).


parse(DCGBody, In) :-
  string_chars(In, Chars),
  parse(DCGBody, Chars, []).

parse(_Module:directive_term(Ops, Tree), In, Out) :-
  !,
  phrase(term(Token_Tree), In, Rest),
  Token_Tree = term(Tokens),
  phrase(end(End_Tree), Rest, Out),
  phrase(term(_Prec, Ops, Term_Tree), Tokens, []),
  Tree = directive_term([Term_Tree, End_Tree]),
  % Condition:
  %   The principal functor is (:-)/1
  principal_functor(Term_Tree, Principal_Functor),
  Principal_Functor = (:-).

parse(_Module:clause_term(Ops, Tree), In, Out) :-
  !,
  phrase(term(Token_Tree), In, Rest),
  Token_Tree = term(Tokens),
  phrase(end(End_Tree), Rest, Out),
  phrase(term(_Prec, Ops, Term_Tree), Tokens, []),
  Tree = clause_term([Term_Tree, End_Tree]),
  % Condition:
  %   The principal functor is not (:-)/1
  principal_functor(Term_Tree, Principal_Functor),
  Principal_Functor \= (:-).

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
