:- module(plammar_operators, [
    merge_operators/3
  ]).

:- use_module(library(plammar/util)).

merge_operators(Old, [], Old).
merge_operators(Old, [OpN|Ops], Merged) :-
  OpN = op(PrecN, Spec, Name),
  OpO = op(PrecO, Spec, Name),
  member(OpO, Old),
  ( 
    PrecN = PrecO,
    merge_operators(Old, Ops, Merged)
  ; 
    !,
    warning('Conflicting operators: ~p, ~p', [OpN, OpO]),
    false
  ), !.
merge_operators(Old, [Op|Ops], Merged) :-
  merge_operators([Op|Old], Ops, Merged).
