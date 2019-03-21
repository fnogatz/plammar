:- module(plammar_ast, [
    prolog_ast/2,
    prolog_ast/3,
    parsetree_ast/2,
    parsetree_ast/3
  ]).

:- use_module(library(plammar)).
:- use_module(library(plammar/util)).
:- use_module(library(plammar/options)).

prolog_ast(Source, AST) :-
  prolog_ast(Source, AST, []).

prolog_ast(Source, AST, Options) :-
  I0 = prolog_parsetree(Source, PT, Options),
  I1 = parsetree_ast(PT, AST, Options),
  ( ground(Source) ->
    Instructions = (I0, I1)
  ; Instructions = (I1, I0) ),
  call(Instructions).

prolog_ast(Source, AST, Options) :-
  \+ var(AST),
  parsetree_ast(PT, AST, Options),
  prolog_parsetree(Source, PT, Options).

parsetree_ast(PT, AST) :-
  parsetree_ast(PT, AST, []).

parsetree_ast(PT, AST, User_Options) :-
  normalise_options(pt_ast, User_Options, Options),
  pt_ast(Options, PT, AST),
  !.

pt_ast(Opts, prolog(PT_List), prolog(AST_List)) :-
  maplist(pt_ast(Opts), PT_List, AST_List).

pt_ast(Opts,
  clause_term([term(PT_Term), end(PT_End)]),
  clause_term(AST_Term)
) :-
  pt_ast(Opts, term(PT_Term), AST_Term),
  append(Layout_Text_Sequence, [end_token(_)], PT_End),
  space(Layout_Text_Sequence, space_before_clause_end, Opts).

pt_ast(Opts, term(atom(PT_Atom)), atom(AST_Atom)) :-
  pt_ast(Opts, atom(PT_Atom), atom(AST_Atom)).

pt_ast(
  Opts,
  term([atom(PT_Atom), open_ct(PT_Open_Ct), arg_list(PT_ArgList), close(PT_Close)]),
  compound(atom(AST_Atom), AST_ArgList)
) :-
  !,
  PT_Open_Ct = open_token(open_char('(')), % no spaces allowed
  pt_ast(Opts, atom(PT_Atom), atom(AST_Atom)),
  % handle layout_text_sequence in front of close
  append(LTS_Close, [close_token(close_char(')'))], PT_Close),
  space(LTS_Close, space_before_close_arglist, Opts),
  pt_ast(Opts, arg_list(PT_ArgList), AST_ArgList).

pt_ast(Opts, arg_list(arg(PT_Arg)), [AST_Arg]) :-
  pt_ast(Opts, arg(PT_Arg), AST_Arg).

pt_ast(Opts, arg(PT_Arg), AST_Arg) :-
  pt_ast(Opts, PT_Arg, AST_Arg).

pt_ast(Opts, atom(name(PT_Name)), atom(AST_Name)) :-
  pt_ast(Opts, name(PT_Name), name(AST_Name)).

pt_ast(_Opts, name([name_token(Atom, _PT_Name_Token)]), name(Atom)) :-
  true. %% TODO: add support for pt_ast(-,+)

pt_ast(Opts, name([layout_text_sequence(_PT_Layout), name_token(A,B)]), AST) :-
  pt_ast(Opts, name([name_token(A,B)]), AST),
  true. %% TODO: handle layout_text_sequence in _PT_Layout

pt_ast(_, Q, Q) :-  !,
  Q =.. [Kind|_],
  setof(
    Type,
    [Opts,A,B,Argument,Body]^(
      clause(pt_ast(Opts,A,B), Body),
      \+ var(A),
      A =.. [Type, Argument]
    ),
    Types 
  ),
  warning('No pt_ast rule defined for ~w. Use one of ~w. Complete call was:~n~w.', [Kind, Types, Q]).

space([], Prop, Options) :-
  style_option(Prop, Expected, Options),
  expected(Prop, Expected, 0, Options).

space([layout_text_sequence([])], Prop, Options) :-
  space([], Prop, Options).

space([layout_text_sequence(List)], Prop, Options) :-
  length(List, List_Length),
  style_option(Prop, Expected, Options),
  expected(Prop, Expected, List_Length, Options).

style_option(Prop, Value, Options) :-
  option(style(Style), Options),
  Style_Prop =.. [Prop, Value],
  option(Style_Prop, Style).

expected(_Prop, Expected, Found, _Options) :-
  Expected == Found,
  !.

expected(_Prop, Expected, Found, Options) :-
  var(Expected),
  !,
  option(style_bind(yes), Options),
  Found = Expected.

expected(Prop, Expected, Found, Options) :-
  option(style_mode(warn), Options),
  !,
  warning('Wrong ~w (found: ~w, expected: ~w)', [Prop, Found, Expected]).
