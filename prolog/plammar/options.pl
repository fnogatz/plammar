:- module(plammar_options, [
    normalise_options/3,
    revise_options/2
  ]).

:- use_module(library(plammar/iso_operators)).
:- use_module(library(plammar/operators)).
:- use_module(library(plammar/util)).

yes(yes).
yes(true).
yes(y).
yes(ok).

normalise_options(prolog_parsetree, User_Options, Options) :-
  default_options(prolog_parsetree, Default_Options),
  merge_options(User_Options, Default_Options, Options0),
  % option: iso_operators
  ( option(iso_operators(Opt_ISO_Operators), Options0), yes(Opt_ISO_Operators) ->
    iso_operators(ISO_Operators)
  ; ISO_Operators = [] ),
  option(operators(User_Operators), Options0),
  merge_operators(ISO_Operators, User_Operators, Operators),
  merge_options([operators(Operators)], Options0, Options1),
  % option: infer_operators
  option(infer_operators(Opt_Infer_Operators), Options1),
  ( yes(Yes), Opt_Infer_Operators == Yes ->
    merge_options([ infer_operators(_) ], Options1, Options2)
  ; Options2 = Options1 % user provided `no` or an unbound variable
  ),
  Options = Options2.

default_options(prolog_parsetree, Options) :-
  Options = [
    operators([]),
    iso_operators(yes),
    infer_operators(no)
  ].

% take the open lists from option `infer_operators`
%   and make them closed
revise_options(prolog_parsetree, Options) :-
  option(infer_operators(Inferred_Ops), Options),
  ( Inferred_Ops == no
  ; list_close(Inferred_Ops)).
