:- use_module(library(plammar/ast)).

'"a." has correct AST' :-
  prolog_asts(string("a."), ASTs, []),
  ASTs = [Single_Result],
  Single_Result = prolog([clause_term(term(atom(name(a))))]).

'" a ." and "a." have identical ASTs' :-
  prolog_ast(string("a."), AST1, []),
  prolog_ast(string(" a ."), AST2, []),
  !,
  AST1 = AST2.

'create "a." by AST' :-
  AST = prolog([clause_term(term(atom(name(a))))]),
  prolog_ast(string(Res), AST, []),
  String = "a.".
