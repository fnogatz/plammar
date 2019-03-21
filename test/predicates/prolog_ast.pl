:- use_module(library(plammar/ast)).

'"a." has correct AST' :-
  prolog_asts(string("a."), ASTs, []),
  ASTs = [Single_Result],
  Single_Result = prolog([clause(atom(a))]).

'" a ." and "a." have identical ASTs' :-
  prolog_ast(string("a."), AST1, []),
  prolog_ast(string("a ."), AST2, []),
  !,
  AST1 = AST2.
/*
'create "a." by AST' :-
  AST = prolog([clause(atom(a))]),
  prolog_ast(string(Res), AST, []),
  String = "a.".
*/

"a." ast prolog([clause(atom(a))]).
