:- module(plammar_environments, [
    target_ops/2,
    target_options/2
  ]).


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

    op( 100, yfx, '.'),  % dicts
    
    op(1150,  fx, 'record'),  % library(record)
    op(1150,  fx, 'rdf_meta'),   % library(semweb)

    % library(clpfd)
    op(760, yfx, '#<==>'),
    op(750, xfy, '#==>'),
    op(750, yfx, '#<=='),
    op(740, yfx, '#\\/'),
    op(730, yfx, '#\\'),
    op(720, yfx, '#/\\'),
    op(710,  fy, '#\\'),
    op(700, xfx, '#>'),
    op(700, xfx, '#<'),
    op(700, xfx, '#>='),
    op(700, xfx, '#=<'),
    op(700, xfx, '#='),
    op(700, xfx, '#\\='),
    op(700, xfx, 'in'),
    op(700, xfx, 'ins'),
    op(450, xfx, '..')

    %% TODO: remove operators of libraries
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
    allow_arg_precedence_geq_1000(yes),
    allow_operator_as_operand(yes)
  ], Options).

target_options(swi(6), Options) :-
  extend_options(swi(5), [], Options).

target_options(swi(5), Options) :-
  extend_options(iso, [
    allow_tab_as_quote_char(yes),
    allow_newline_as_quote_char(yes),
    allow_c_as_continuation_escape_symbol(yes),
    allow_unicode(yes)
  ], Options).


target_options(iso, Options) :-
  Options = [
    allow_unicode(no),
    var_prefix(no),
    dicts(no),
    back_quoted_text(no),
    allow_arg_precedence_geq_1000(no),
    allow_operator_as_operand(no),
    allow_tab_as_quote_char(no),
    allow_newline_as_quote_char(no),
    allow_c_as_continuation_escape_symbol(no)
  ].

extend_options(Target, Extension, Combined) :-
  target_options(Target, Target_Options),
  merge_options(Extension, Target_Options, Combined).
