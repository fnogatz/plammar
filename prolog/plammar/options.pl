:- module(plammar_options, [
    normalise_options/2,
    normalise_options/3,
    revise_options/2,
    style_option/2
  ]).

:- use_module(library(plammar/operators)).
:- use_module(library(plammar/environments)).
:- use_module(library(plammar/util)).

normalise_options(User_Options, Options) :-
  O1 = User_Options,
  normalise_options(prolog_tokens, O1, O2),
  normalise_options(prolog_parsetree, O2, O3),
  normalise_options(prolog_ast, O3, O4),
  Options = O4.

normalise_options(prolog_tokens, User_Options, Options) :-
  option(targets(Targets), User_Options, [iso]),
  ( Targets = [] ->
    Target = iso
  ; Targets = [Target|_] ),
  target_options(Target, Target_Options),
  merge_options(User_Options, Target_Options, Options0),
  Options = Options0.

normalise_options(prolog_parsetree, User_Options, Options) :-
  default_options(prolog_parsetree, Options0),
  % merge target options
  option(targets(Targets), User_Options, [iso]),
  ( Targets = [] ->
    Target = iso
  ; Targets = [Target|_] ),
  target_options(Target, Target_Options),
  merge_options(Target_Options, Options0, Options1),
  % merge user options
  merge_options(User_Options, Options1, Options2),
  % option: infer_operators
  option(infer_operators(Opt_Infer_Operators), Options2),
  ( yes(Yes), Opt_Infer_Operators == Yes ->
    merge_options([ infer_operators(_) ], Options2, Options3)
  ; Options3 = Options2 % user provided `no` or an unbound variable
  ),
  % option: operators
  option(operators(Operators0), Options3),
  normalise_operators(Operators0, Operators1),
  merge_options([operators(Operators1)], Options3, Options4),
  Options = Options4.

normalise_options(prolog_ast, User_Options, Options) :-
  default_options(prolog_ast, Default_Options),
  merge_options(User_Options, Default_Options, Options0),
  option(style(User_Style), Options0, []),
  normalise_style(User_Style, Style),
  merge_options([style(Style)], Options0, Options1),
  Options = Options1.

default_options(prolog_parsetree, Options) :-
  Options = [
    operators([]),
    specified_operators(L),
    infer_operators(no),
    targets([iso]),
    allow_variable_name_as_functor(no),
    arg_precedence_lt_1000(yes)
  ],
  list_open([], L).

default_options(prolog_ast, Options) :-
  Options = [
    end_state(_)
  ].

revise_options(prolog_tokens, _).

% take the open lists from option `infer_operators`
%   and make them closed
revise_options(prolog_parsetree, Options) :-
  option(infer_operators(Inferred_Ops), Options),
  ( no(No), Inferred_Ops == No
  ; list_close(Inferred_Ops)).

normalise_style(User_Style, Style) :-
  default_style(Default_Style),
  maplist(add_secondary, User_Style, User_Style_With_Secondaries),
  merge_options(User_Style_With_Secondaries, Default_Style, Style).

default_style(Style) :-
  Style0 = [
    % general formatting
    indent(2),
    max_line_length(78),
    no_eol_whitespace(yes),

    % format of rules
    max_subgoals(inf),
    max_rule_lines(inf),
    newline_after_rule_op(yes),
    newline_after_subgoal(yes),

    % format of clauses
    newline_after_clause(yes),

    % predicates
    space_after_arglist_comma(yes),

    % format of numbers
    integer_exponential_notation(no),
    digit_groups(no), % one of: [no, '_', ' ']
    single_quote_char_in_character_code_constant(no),

    % format of quoted items
    tab_in_quotes(no),
    missing_closing_backslash_in_character_escape(no),
    unicode_character_escape(yes),

    % dicts
    dicts(yes),

    shebang(no),
    compounds_with_zero_arguments(no),

    symbolic_chars(yes)
  ],
  maplist(add_secondary, Style0, Style).

add_secondary(Opt, Opt_With_Secondary) :-
  Opt =.. [Prop, Value|Possibly_Secondary],
  ( Possibly_Secondary = [] ->
    Secondary = []
  ; otherwise ->
    Possibly_Secondary = [Secondary]
  ),
  Opt_With_Secondary =.. [Prop, Value/Secondary].

style_option(Prop, Opts) :-
  option(style(Style), Opts),
  option(Prop, Style).
