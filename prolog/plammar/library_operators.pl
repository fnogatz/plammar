:- module(plammar_library_operators, [
    library_operators/2
  ]).

library_operators(clpfd, Ops) :-
  Ops = [
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
  ].

library_operators(record, Ops) :-
  Ops = [
    op(1150, fx, 'record')
  ].

library_operators(semweb, Ops) :-
  Ops = [
    op( 110, xfx, @),
    op( 650, xfx, ^^),
    op(1150,  fx, 'rdf_meta')
  ].
library_operators(semweb/_, Ops) :-
  library_operators(semweb, Ops).

library_operators(lib, Ops) :-
  Ops = [
    op(200, fy, '&')
  ].

library_operators(Lib, Ops) :-
  member(Lib, [dcg_macros, dcg_pair, dcg_core, dcg_codes]),
  Ops = [
    op(900,  fy, '\\<'),
    op(900,  fy, '\\>'),
    op(900, xfx, '<\\>'),
    op(900,  fy, '<\\>'),
    op(900, xfy, '\\#')
  ].

library_operators(xpath, Ops) :-
  Ops = [
    op(400, fx, '//'),
    op(400, fx, '/'),
    op(200, fy, '@')
  ].

library_operators(real, Ops) :-
  Ops = [
    op(950, fx, '<-'),
    op(950, yfx, '<-'),
    op(950, yfx, '<<-'),
    op(950, xf, '<<-')
  ].

library_operators(chr, Ops) :-
  Ops = [
    op(1180, xfx, '==>'),
    op(1180, xfx, '<=>'),
    op(1150,  fx, 'constraints'),
    op(1150,  fx, 'chr_constraint'),
    op(1150,  fx, 'handler'),
    op(1150,  fx, 'rules'),
    op(1100, xfx, '\\'),
    op(1200, xfx, '@'),
    op(1190, xfx, 'pragma'),
    op( 500, yfx, '#'),
    op(1150,  fx, 'chr_type'),
    op(1130, xfx, '--->'),
    op(1150,  fx, '(?)'),
    op(1150,  fx, 'chr_declaration')
  ].

library_operators(lambda, Ops) :-
  Ops = [
    op(201,xfx,'+\\')
  ].

library_operators(clambda, Ops) :-
  Ops = [
    op(201,xfx,'+\\')
  ].

library_operators(typedef, Ops) :-
  Ops = [
    op(1150,fx,'type'),
    op(1130,xfx,'--->')
  ].

library_operators(http/html_write, Ops) :-
  Ops = [
    op(1150, fx, 'html_meta')
  ].

library_operators(assertions, Ops) :-
  Ops = [
    op(975,  xfx, '=>'),
    op(978,  xfx, '::'),
    op(1150,  fx, 'decl'),
    op(1150, xfx, 'decl'),
    op(1150,  fx, 'pred'),
    op(1150, xfx, 'pred'),
    op(1150,  fx, 'prop'),
    op(1150, xfx, 'prop'),
    op(1150,  fx, 'modedef'),
    op(1150,  fx, 'calls'),
    op(1150, xfx, 'calls'),
    op(1150,  fx, 'success'),
    op(1150, xfx, 'success'),
    op(1150,  fx, 'comp'),
    op(1150, xfx, 'comp'),
    op(1150,  fx, 'entry'),
    op(1150,  fx, 'exit'),
    op(1150, xfx, 'exit'),
    op( 550, yfx, '#'),
    op( 500,  fx, '?'),
    op( 500,  fx, ':'),
    op( 500,  fx, '='),
    op( 500,  fx, '/'),
    op( 500,  fx, '>'),
    op( 500,  fx, '!'),
    op( 500,  fx, '*'),
    op( 500,  fx, '@'),
    op(1150,  fx, 'type')
  ].

library_operators(ffi, Ops) :-
  Ops = [
    op(200,  fy, '*'),
    op(100, xfx, '~'),
    op( 20, yf , '[]')
  ].

library_operators(func, Ops) :-
  Ops = [
    op(675, xfy, '$'),
    op(650, xfy, 'of')
  ].

library_operators(type_check, Ops) :-
  Ops = [
    op(1150,  fx, 'type'),
    op(1130, xfx, '--->'),
    op(1150,  fx, 'pred'),
    op(1150,  fx, 'trust_pred'),
    op( 500, yfx, '::'),
    op( 500, yfx, ':<'),
    op( 200,  fy, '?')
  ].

library_operators(dictoo, Ops) :-
  Ops = [
    op(2,fx,'?')
  ].

library_operators(regex, Ops) :-
  Ops = [
    op(700, xfx, '=~'),
    op(700, xfx, '\\~')
  ].

library_operators(swipe, Ops) :-
  Ops = [
    op(300, xfy, ':>'),
    op(300, yfx, '>:'),
    op(200,  fy, '@')
  ].
