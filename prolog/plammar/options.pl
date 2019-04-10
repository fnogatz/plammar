:- module(plammar_options, [
    normalise_options/2,
    normalise_options/3,
    revise_options/2
  ]).

:- use_module(library(plammar/operators)).
:- use_module(library(plammar/environments)).
:- use_module(library(plammar/util)).

normalise_options(User_Options, Options) :-
  O1 = User_Options,
  normalise_options(prolog_tokens, O1, O2),
  normalise_options(prolog_parsetree, O2, O3),
  Options = O3.

normalise_options(prolog_tokens, User_Options, Options) :-
  option(targets(Targets), User_Options, [iso]),
  ( Targets = [] ->
    Target = iso
  ; Targets = [Target|_] ),
  target_options(Target, Target_Options),
  merge_options(User_Options, Target_Options, Options0),
  Options = Options0.

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
    infer_operators(no),
    allow_variable_name_as_functor(no)
  ].

revise_options(prolog_tokens, _).

% take the open lists from option `infer_operators`
%   and make them closed
revise_options(prolog_parsetree, Options) :-
  option(infer_operators(Inferred_Ops), Options),
  ( no(No), Inferred_Ops == No
  ; list_close(Inferred_Ops)).
