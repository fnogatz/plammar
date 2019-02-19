:- module(plammar_options, [
    normalise_options/3
  ]).

:- use_module(library(plammar/iso_operators)).

normalise_options(prolog_parsetree, User_Options, Options) :-
  default_options(prolog_parsetree, Default_Options),
  merge_options(User_Options, Default_Options, Options).

default_options(prolog_parsetree, Options) :-
  iso_operators(ISO_Operators),
  Options = [
    operators(ISO_Operators)
  ].
