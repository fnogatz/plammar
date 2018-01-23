:- consult('pack.pl').
:- use_module('prolog/ast').

opts_spec([
  [
    opt(help),
    type(boolean),
    default(false),
    shortflags([ h ]),
    longflags([ help ]),
    help([
      'display this help'
    ])
  ],
  [
    opt(version),
    type(boolean),
    default(false),
    shortflags([ v, 'V' ]),
    longflags([ version ]),
    help([
      'display version'
    ])
  ],
  [
    opt(dcg),
    type(atom),
    default(prolog),
    shortflags([ d ]),
    longflags([ dcg ]),
    help([
      'start from this DCG body'
    ])
  ],
  [
    opt(pretty),
    type(boolean),
    default(false),
    shortflags([ p ]),
    longflags([ pretty ]),
    help([
      'pretty output'
    ])
  ]
]).

main :-
  opts_spec(OptsSpec),
  opt_arguments(OptsSpec,Opts,PositionalArgs),
  main(Opts,PositionalArgs).

main(Opts,_PositionalArgs) :-
  memberchk(version(true),Opts),
  !,
  version(V),
  writeln(V),
  halt(0).

main(Opts,_PositionalArgs) :-
  memberchk(help(true),Opts),
  !,
  opts_spec(OptsSpec),
  opt_help(OptsSpec,Help),
  writeln('USAGE: plammar [options] [<file>]'), nl,
  writeln('Parse an input as Prolog source code'),
  writeln('If no input was given it reads from stdin.'), nl,
  writeln('Options:'),
  writeln(Help),
  halt(0).

main(Opts,PositionalArgs) :-
  PositionalArgs = [Filename],
  ( from_file(Opts,Filename) ->
    success
  ; no_success ).

main(Opts,PositionalArgs) :-
  PositionalArgs = [],
  ( from_stdin(Opts) ->
    success
  ; no_success ).

no_success :-
  writeln('no'),
  halt(1).

success :-
  writeln('yes'),
  halt(0).

success(Opts, Result) :-
  memberchk(pretty(Pretty), Opts),
  (
    Pretty = true,
    print_term(Result, [
      indent_arguments(2)
    ]),
    nl
  ;
    Pretty = false,
    writeln(Result)
  ),
  halt(0).

from_file(Opts, Filename) :-
  memberchk(dcg(Start_Body), Opts),
  ( ast:tree_from_file(Start_Body, Filename, Tree) ->
    success(Opts, Tree)
  ; no_success ).

from_stdin(Opts) :-
  memberchk(dcg(Start_Body), Opts),
  repeat,
  read_line_to_codes(user_input, Codes),
  maplist(char_code, Chars, Codes),
  ( ast:tree(Start_Body, Chars, Tree) ->
    success(Opts, Tree)
  ; no_success ).
