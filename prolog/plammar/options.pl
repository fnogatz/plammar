:- module(plammar_options, [
    normalise_options/3,
    revise_options/2
  ]).

:- use_module(library(plammar/operators)).
:- use_module(library(plammar/util)).

yes(yes).
yes(true).
yes(y).
yes(ok).

normalise_options(prolog_parsetree, User_Options, Options) :-
  default_options(prolog_parsetree, Default_Options),
  merge_options(User_Options, Default_Options, Options0),
  % option: infer_operators
  option(infer_operators(Opt_Infer_Operators), Options0),
  ( yes(Yes), Opt_Infer_Operators == Yes ->
    merge_options([ infer_operators(_) ], Options0, Options1)
  ; Options1 = Options0 % user provided `no` or an unbound variable
  ),
  Options = Options1.

default_options(prolog_parsetree, Options) :-
  Options = [
    operators([]),
    targets([iso]),
    infer_operators(no)
  ].

% take the open lists from option `infer_operators`
%   and make them closed
revise_options(prolog_parsetree, Options) :-
  option(infer_operators(Inferred_Ops), Options),
  ( Inferred_Ops == no
  ; list_close(Inferred_Ops)).
