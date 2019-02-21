:- use_module(library(plammar/ast)).

'"a." has correct AST' :-
  prolog_asts(string("a."), ASTs, []),
  ASTs = [Single_Result],
  Single_Result = p_text([clause_term(term(atom(name(a))))]).

'" a ." has correct AST' :-
  prolog_asts(string(" a ."), ASTs, []),
  ASTs = [Single_Result],
  Single_Result = p_text([clause_term(term(atom(name(a))))]).
