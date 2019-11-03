:- module(plammar_environments, [
    target_ops/2,
    target_options/2
  ]).

:- use_module(library(lists), [append/3]).
:- use_module(library(option), [merge_options/3]).

%% Operators for Environments

target_ops(iso, Ops) :-
  Ops = [
    op(1200, xfx, ':-'),
    op(1200, xfx, '-->'),
    op(1200,  fx, ':-'),
    op(1200,  fx, '?-'),
    op(1100, xfy, ';'),
    op(1050, xfy, '->'),
    op(1000, xfy, ','),
    op( 900,  fy, '\\+'),
    op( 700, xfx, '='),
    op( 700, xfx, '\\='),
    op( 700, xfx, '=='),
    op( 700, xfx, '\\=='),
    op( 700, xfx, '@<'),
    op( 700, xfx, '@=<'),
    op( 700, xfx, '@>'),
    op( 700, xfx, '@>='),
    op( 700, xfx, '=..'),
    op( 700, xfx, 'is'),
    op( 700, xfx, '=:='),
    op( 700, xfx, '=\\='),
    op( 700, xfx, '<'),
    op( 700, xfx, '=<'),
    op( 700, xfx, '>'),
    op( 700, xfx, '>='),
    op( 500, yfx, '+'),
    op( 500, yfx, '-'),
    op( 500, yfx, '/\\'),
    op( 500, yfx, '\\/'),
    op( 400, yfx, '*'),
    op( 400, yfx, '/'),
    op( 400, yfx, '//'),
    op( 400, yfx, 'div'),  % ISO 13211 Corr. 2
    op( 400, yfx, 'rem'),
    op( 400, yfx, 'mod'),
    op( 400, yfx, '<<'),
    op( 400, yfx, '>>'),
    op( 200, xfx, '**'),
    op( 200, xfy, '^'),
    op( 200,  fy, '+'),    % ISO 13211 Corr. 2
    op( 200,  fy, '-'),
    op( 200,  fy, '\\')
  ].

target_ops(swi, Ops) :-
  extend_ops(iso, [
    op(1105, xfy, '|'),
    op(1050, xfy, '*->'),
    op( 700, xfx, '>:<'),
    op( 700, xfx, ':<'),
    op( 700, xfx, '=@='),
    op( 700, xfx, '\\=@='),
    op( 600, xfy, ':'),
    op( 400, yfx, 'rdiv'),
    op( 400, yfx, 'xor'),
    op(1150,  fx, 'discontiguous'),
    op(1150,  fx, 'dynamic'),
    op(1150,  fx, 'volatile'),
    op(1150,  fx, 'thread_local'),
    op(1150,  fx, 'initialization'),
    op(1150,  fx, 'thread_initialization'),
    op(1150,  fx, 'module_transparent'),
    op(1150,  fx, 'multifile'),
    op(1150,  fx, 'meta_predicate'),
    op(1150,  fx, 'public'),
    op(1150,  fx, 'table'),
    op( 700, xfx, 'as'),

    op( 200,  fy, '@'),  % no idea where this comes from...
    op(   1,  fx, '$'),  % no idea where this comes from...
    op( 990, xfx, ':='), % no idea where this comes from, was there before dicts were introduced...

    op( 100, yfx, '.')  % dicts
  ], Ops).

extend_ops(Target, Extension, Combined) :-
  target_ops(Target, Target_Ops),
  append(Target_Ops, Extension, Combined).


%% Options for Environments

target_options(swi, Options) :-
  target_options(swi(8), Options).

target_options(swi(8), Options) :-
  extend_options(swi(7), [], Options).

target_options(swi(7), Options) :-
  extend_options(swi(6), [
    dicts(yes),
    back_quoted_text(yes),
    allow_compounds_with_zero_arguments(yes),
    allow_arg_precedence_geq_1000(yes),
    allow_operator_as_operand(yes)
  ], Options).

target_options(swi(6), Options) :-
  extend_options(swi(5), [
    allow_digit_groups_with_underscore(yes),
    allow_digit_groups_with_space(yes)
  ], Options).

target_options(swi(5), Options) :-
  extend_options(iso, [
    allow_shebang(yes),
    allow_tab_as_quote_char(yes),
    allow_newline_as_quote_char(yes),
    allow_symbolic_no_output_char_c(yes),
    allow_symbolic_escape_char_e(yes),
    allow_symbolic_space_char_s(yes),
    allow_unicode(yes),
    allow_integer_exponential_notation(yes),
    allow_single_quote_char_in_character_code_constant(yes),
    allow_missing_closing_backslash_in_character_escape(yes),
    allow_unicode_character_escape(yes)
  ], Options).


target_options(iso, Options) :-
  Options = [
    allow_shebang(no),
    allow_unicode(no),
    var_prefix(no),
    dicts(no),
    back_quoted_text(no),
    allow_compounds_with_zero_arguments(no),
    allow_arg_precedence_geq_1000(no),
    allow_operator_as_operand(no),
    allow_tab_as_quote_char(no),
    allow_newline_as_quote_char(no),
    allow_symbolic_no_output_char_c(no),
    allow_symbolic_escape_char_e(no),
    allow_symbolic_space_char_s(no),
    allow_digit_groups_with_underscore(no),
    allow_digit_groups_with_space(no),
    allow_integer_exponential_notation(no),
    allow_single_quote_char_in_character_code_constant(no),
    allow_missing_closing_backslash_in_character_escape(no),
    allow_unicode_character_escape(no)
  ].

extend_options(Target, Extension, Combined) :-
  target_options(Target, Target_Options),
  merge_options(Extension, Target_Options, Combined).
