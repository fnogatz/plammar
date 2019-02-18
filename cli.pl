:- consult('pack.pl').
:- use_module(library(plammar)).
:- use_module(library(clitable)).

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
  ],
  [
    opt(ops),
    type(term),
    longflags([ ops ]),
    default([]),
    help([
      'pre-defined operators'
    ])
  ],
  [
    opt(nots),
    type(term),
    longflags([ 'not-ops' ]),
    help([
      'disallow operators'
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

main(Opts, [Filename]) :-
  read_file_to_codes(Filename, Codes, []),
  maplist(char_code, Chars, Codes),
  process(Opts, Chars).

main(Opts, []) :-
  collect_stdin(Chars),
  process(Opts, Chars).

no_success :-
  writeln('no'),
  halt(1).

success :-
  writeln('yes'),
  halt(0).

success(Opts, Result) :-
  write_result(Opts, Result),
  halt(0).

write_result(Opts, Result) :-
  option(pretty(Pretty), Opts),
  (
    Pretty = true,
    print_term(Result, [
      indent_arguments(2)
    ]),
    nl
  ;
    Pretty = false,
    writeln(Result)
  ).

collect_stdin(Chars) :-
  repeat,
  read_line_to_codes(user_input, Codes),
  maplist(char_code, Chars, Codes).

process(Opts, Chars) :-
  option(dcg(DCGBody), Opts),
  \+var(DCGBody),
  ( ast:tree(DCGBody, Chars, Tree) ->
    success(Opts, Tree)
  ; no_success ).

process(Opts, Chars) :-
  option(dcg(DCGBody), Opts),
  var(DCGBody),

  option(ops(User_Ops), Opts),
  option(nots(Nots), Opts),

  iso_operators(ISO_Ops),
  append(ISO_Ops, User_Ops, Ops),

  !,
  ( ast:prolog(ops(Ops, Nots), AST, Chars),
    print_result(Opts, 0, Ops, Nots, AST, Chars),
    false
  ; halt(0) ).


/*
process(Opts, Chars) :-
  option(dcg(DCGBody), Opts),
  var(DCGBody),

  option(ops(Ops), Opts),
  option(nots(Nots), Opts),

  forall(
    ast:parse(term(Prec, ops(Ops,Nots), AST), Chars),
    print_result(Prec, Ops, Nots, AST, Chars)
  ).
*/

print_result(Opts, _Prec, Ops, Nots, AST, _Chars) :-
  writeln('--------------------------------'), nl,
%  ansi_format([bold,fg(blue)], 'Precedence: ', []),
%  precedence_output(Prec, Prec_, _),
%  ansi_format([], '~w', [Prec_]),
%  nl, nl,
  ansi_format([bold, fg(green)], 'Operators:~n', []),
  print_operators(Opts, Ops),
  nl,
  ansi_format([bold, fg(red)], 'Not Operators:~n', []),
  print_operators(Opts, Nots),
  nl,
  ansi_format([bold, fg(blue)], 'Syntax Tree:~n', []),
  write_result(Opts, AST),
  nl,
  !.

print_operators(_Opts, []) :-
  !,
  writeln('(none)').
print_operators(Opts, Ops) :-
  maplist_op_entry(Opts, Ops, Op_List),
  clitable(Op_List, [head(['Precedence', 'Type', 'Name'])]).

maplist_op_entry(_Opts, Xs, []) :-
  var(Xs),
  !.
maplist_op_entry(_Opts, [], []) :- !.
maplist_op_entry(Opts, [X|Xs], Ys) :-
  % do not show iso_operators
  X = op(Prec, Spec, Name),
  iso_operator(Prec, Spec, Name),
  !,
  maplist_op_entry(Opts, Xs, Ys).
maplist_op_entry(Opts, [X|Xs], [Y|Ys]) :-
  op_entry(X, Y),
  maplist_op_entry(Opts, Xs, Ys).


op_entry(op(Prec, Spec, Name), [Prec_, Spec_, Name_]) :-
  ( var(Name) -> Name_ = '*' ; Name_ = Name ),
  precedence_output(Prec, Prec_, Name_),
  ( var(Spec) -> Spec_ = '*' ; Spec_ = Spec ).

precedence_output(Prec, Res, _) :-
  number(Prec), !,
  Res = Prec.
precedence_output(Prec, Res, Name) :-
  attvar(Prec),
  get_attr(Prec, clpfd, Attr),
  Attr = clpfd_attr(_,_,_,from_to(n(From), n(To)), _),
  ( var(Name) -> Name_ = 'P' ; format(atom(Name_), 'P(~w)', [Name])),
  format(atom(Res), '~d =< ~w =< ~d', [From, Name_, To]).

precedence_output(_Prec, '*', _).
