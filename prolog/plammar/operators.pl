:- module(plammar_operators, [
    merge_operators/3,
    normalise_operators/2
  ]).

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(apply), [maplist/3]).

:- use_module(util).

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

normalise_operators([], []).
normalise_operators([Op0|Ops0], Ops) :-
  normalise_operator(Op0, List),
  normalise_operators(Ops0, Ops1),
  append(List, Ops1, Ops).

normalise_operator(Op, Res) :-
  Op = op(Prec, Spec, A),
  ( is_list(A) ->
    maplist(build_op(Prec, Spec), A, Res)
  ; otherwise ->
    Res = [Op]
  ).

build_op(Prec, Spec, A, op(Prec, Spec, A)).
