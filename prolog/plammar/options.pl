:- module(plammar_options, [
    normalise_options/3
  ]).

:- use_module(library(plammar/iso_operators)).

normalise_options(prolog_parsetree, User_Options, Options) :-
  default_options(prolog_parsetree, Default_Options),
  merge_options(User_Options, Default_Options, Options0),
  ( option(iso_operators(X), Options0), yes(X) ->
    iso_operators(ISO_Operators),
    option(operators(User_Operators), Options0),
    merge_operators(ISO_Operators, User_Operators, Operators),
    merge_options([operators(Operators)], Options0, Options1)
  ; Options1 = Options0 ),
  Options = Options1.

default_options(prolog_parsetree, Options) :-
  Options = [
    operators([]),
    iso_operators(yes),
    infer_operators(no)
  ].

yes(yes).
yes(true).
yes(y).
yes(ok).

merge_operators(Old, New, Merged) :-
  %% TODO!
  append(Old, New, Merged).
