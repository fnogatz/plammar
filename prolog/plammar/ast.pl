:- module(plammar_ast, [
    prolog_ast/2,
    prolog_ast/3,
    parsetree_ast/2,
    parsetree_ast/3
  ]).

:- use_module(library(plammar)).
:- use_module(library(plammar/util)).

prolog_ast(Source, AST) :-
  prolog_ast(Source, AST, []).

prolog_ast(Source, AST, Options) :-
  I0 = prolog_parsetree(Source, PT, Options),
  I1 = parsetree_ast(PT, AST, Options),
  ( ground(Source) ->
    Instructions = (I0, I1)
  ; Instructions = (I1, I0) ),
  call(Instructions).

prolog_ast(Source, AST, Options) :-
  \+ var(AST),
  parsetree_ast(PT, AST, Options),
  prolog_parsetree(Source, PT, Options).

parsetree_ast(PT, AST) :-
  parsetree_ast(PT, AST, []).

parsetree_ast(PT, AST, Options) :-
  pt_ast(Options, PT, AST).

pt_ast(Opts, p_text(PT_List), p_text(AST_List)) :-
  maplist(pt_ast(Opts), PT_List, AST_List).

pt_ast(Opts, clause_term([term(PT_Term), end(_PT_End)]), clause_term(term(AST_Term))) :-
  pt_ast(Opts, PT_Term, AST_Term),
  true. %% TODO: handle layout_text_sequence in PT_End

pt_ast(Opts, atom(name(PT_Name)), atom(name(AST_Name))) :-
  pt_ast(Opts, name(PT_Name), name(AST_Name)).

pt_ast(_Opts, name([name_token(Atom, _PT_Name_Token)]), name(Atom)) :-
  true. %% TODO: add support for pt_ast(-,+)

pt_ast(Opts, name([layout_text_sequence(_PT_Layout), name_token(A,B)]), AST) :-
  pt_ast(Opts, name([name_token(A,B)]), AST),
  true. %% TODO: handle layout_text_sequence in _PT_Layout

pt_ast(_, Q, Q) :-  !,
  Q =.. [Kind|_],
  setof(
    Type,
    [Opts,A,B,Argument,Body]^(
      clause(pt_ast(Opts,A,B), Body),
      \+ var(A),
      A =.. [Type, Argument]
    ),
    Types 
  ),
  warning('No pt_ast rule defined for ~w. Use one of ~w', [Kind, Types]).
