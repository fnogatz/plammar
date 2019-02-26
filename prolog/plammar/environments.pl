:- module(plammar_environments, [
      target_ops/2
   ]).

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
    op( 400, yfx, 'rem'),
    op( 400, yfx, 'mod'),
    op( 400, yfx, '<<'),
    op( 400, yfx, '>>'),
    op( 200, xfx, '**'),
    op( 200, xfy, '^'),
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
    op( 400, yfx, 'div'),
    op( 400, yfx, 'rdiv'),
    op( 400, yfx, 'xor'),
    op( 200,  fy, '+'),
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
    op( 700, xfx, 'as')
  ], Ops).

extend_ops(Target, Extension, Combined) :-
  target_ops(Target, Target_Ops),
  append(Target_Ops, Extension, Combined).
