:- module(plammar_iso_operators, [
      iso_operator/3,
      iso_operators/1
   ]).

iso_operator(1200, xfx, ':-').
iso_operator(1200, xfx, '-->').
iso_operator(1200,  fx, ':-').
iso_operator(1200,  fx, '?-').
iso_operator(1100, xfy, ';').
iso_operator(1050, xfy, '->').
iso_operator(1000, xfy, ',').
iso_operator( 900,  fy, '\\+').
iso_operator( 700, xfx, '=').
iso_operator( 700, xfx, '\\=').
iso_operator( 700, xfx, '==').
iso_operator( 700, xfx, '\\==').
iso_operator( 700, xfx, '@<').
iso_operator( 700, xfx, '@=<').
iso_operator( 700, xfx, '@>').
iso_operator( 700, xfx, '@>=').
iso_operator( 700, xfx, '=..').
iso_operator( 700, xfx, 'is').
iso_operator( 700, xfx, '=:=').
iso_operator( 700, xfx, '=\\=').
iso_operator( 700, xfx, '<').
iso_operator( 700, xfx, '=<').
iso_operator( 700, xfx, '>').
iso_operator( 700, xfx, '>=').
iso_operator( 500, yfx, '+').
iso_operator( 500, yfx, '-').
iso_operator( 500, yfx, '/\\').
iso_operator( 500, yfx, '\\/').
iso_operator( 400, yfx, '*').
iso_operator( 400, yfx, '/').
iso_operator( 400, yfx, '//').
iso_operator( 400, yfx, 'rem').
iso_operator( 400, yfx, 'mod').
iso_operator( 400, yfx, '<<').
iso_operator( 400, yfx, '>>').
iso_operator( 200, xfx, '**').
iso_operator( 200, xfy, '^').
iso_operator( 200,  fy, '-').
iso_operator( 200,  fy, '\\').

iso_operators(Ops) :-
  findall(op(Prec,Spec,Name), iso_operator(Prec,Spec,Name), Ops).
