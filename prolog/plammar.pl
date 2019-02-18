:- module(plammar, [
    tree/3,
    tree/4,
    parse/2,
    parse/3,
    prolog/3,
    prolog/4,
    prolog_tokens/2,
    prolog_parsetree/2,
    prolog_parsetree/3
  ]).

:- reexport(library(plammar/iso_operators)).
:- use_module(library(plammar/util)).

:- use_module(library(clpfd)).

prolog_tokens(string(String), Tokens) :-
  \+ var(String),
  !,
  string_chars(String, Chars),
  phrase(plammar:term(term(Tokens)), Chars, []).
prolog_tokens(string(String), Tokens) :-
  \+ var(Tokens),
  !,
  phrase(plammar:term(term(Tokens)), Chars, []),
  string_chars(String, Chars).
prolog_tokens(string(String), Tokens) :-
  var(String), var(Tokens),
  !,
  warning('Either string or tokens must be given.'),
  fail.

prolog_tokens(chars(Chars), Tokens) :-
  phrase(plammar:term(term(Tokens)), Chars, []).

prolog_tokens(_, _) :-
  !,
  setof(
    Type,
    [Selector,Argument,Body,A]^(
      clause(prolog_tokens(Selector,A), Body),
      \+ var(Selector),
      Selector =.. [Type, Argument]
    ),
    Types 
  ),
  warning('Use one of input formats string ~w', Types).

prolog_parsetree(A, B) :-
  prolog_parsetree(A, B, []).

prolog_parsetree(string(String), PT, Options) :-
  \+ var(String),
  !,
  string_chars(String, Chars),
  prolog_parsetree(chars(Chars), PT, Options).
prolog_parsetree(string(String), PT, Options) :-
  \+ var(PT),
  !,
  prolog_parsetree(chars(Chars), PT, Options),
  string_chars(String, Chars).

prolog_parsetree(chars(Chars), p_text(P_Text_List), Options) :-
  !,
  option(ops(Ops), Options, _),
  iso_operators(Iso_Ops),
  list_open(Iso_Ops, Ops),
  p_text(ops(Ops, Nops), p_text(P_Text_List), Chars, []).

prolog_parsetree(_, _, _Options) :-
  !,
  setof(
    Type,
    [Selector,Argument,Body,A,B]^(
      clause(prolog_parsetree(Selector,A,B), Body),
      \+ var(Selector),
      Selector =.. [Type, Argument]
    ),
    Types 
  ),
  warning('Use one of input formats ~w', [Types]).


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

prolog(Ops, p_text(P_Text_List_With_Ws), In, Out) :-
  p_text(Ops, p_text(P_Text_List), In, Rest),
  % there might be trailing white space
  phrase(?(plammar:layout_text_sequence, Layout_Tree), Rest, Out),
  (  Layout_Tree = []
  -> P_Text_List_With_Ws = P_Text_List
  ;  append(P_Text_List, Layout_Tree, P_Text_List_With_Ws) ).


p_text(_Ops, p_text([]), In, In).

p_text(Ops, p_text([Clause_Tree|Rec]), In, Out) :-
  parse_prolog_term(Ops, Clause_Tree, In, Rest),
  p_text(Ops, p_text(Rec), Rest, Out).


parse_prolog_term(Ops, Tree, In, Out) :-
  append(First, ['.'|Out], In),
  phrase(term(Token_Tree), First, FirstRest),
  % FirstRest might be `?layout_text_sequence` (6.4)
  phrase(?(plammar:layout_text_sequence, Layout_Tree), FirstRest, []),
  Token_Tree = term(Tokens),
  phrase(term(_Prec, Ops, Term_Tree), Tokens, []),
  % Check whether the principal functor is (:-)/1 or not
  principal_functor(Term_Tree, Principal_Functor),
  (  Principal_Functor = (:-)
  -> Tree_Name = directive_term
  ;  Tree_Name = clause_term ),
  % Build resulting parse tree
  End_Tree_List = [end_token(end_char('.'))],
  (  Layout_Tree = []
  -> End_Tree_With_Layout = end(End_Tree_List)
  ;  append(Layout_Tree, End_Tree_List, End_Tree_List_With_Layout),
     End_Tree_With_Layout = end(End_Tree_List_With_Layout) ),
  Tree =.. [Tree_Name, [Term_Tree, End_Tree_With_Layout]].


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
  \+ var(In), !,
  token_(token_(Tree), In, Rest),
  Some_More_Elements = [_|_], % at least one element
  \+((
    token_(_, In, Shorter_Rest),
    append(Some_More_Elements, Shorter_Rest, Rest)
  )).
token(Tree, In, Rest) :-
  \+ var(Tree), !,
  token_(token_(Tree), In, Rest).
token(Tree, In, Rest) :-
  var(Tree), var(In), !,
  warning('Parse tree AND input unbound; this might not work as expected!'),
  token_(token_(Tree), In, Rest).

:- use_module(library(dcg4pt/expand)).

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

:- load_files('plammar/dcg_token.pl', [module(plammar)]).
:- load_files('parser.pl', [module(plammar)]).
