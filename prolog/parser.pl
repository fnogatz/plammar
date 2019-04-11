:- module(parser, []).

:- use_module(library(yall)).
:- use_module(library(plammar/environments)).

:- style_check(-singleton).

is_priority(P) :-
  P #>= 0,
  P #=< 1201.

is_operator(Op0, Options) :-
  Op0 = op(Prec, Spec, Name0),
  % remove leading space if present
  ( atom_concat(' ', Name, Name0) ; Name = Name0 ), !,
  Op = op(Prec, Spec, Name),
  is_priority(Prec),
  
  % get relevant options
  option(specified_operators(Specified_Operators), Options, _),
  option(operators(Operators), Options),
  option(targets(Targets), Options),
  option(infer_operators(Inferred_Ops), Options),
  
  ( % Option I: it is part of the specified_operators(_) option
    %   of operators given in the source code
    open_member(Op, Specified_Operators)
  ; % Option II: it is part of the operators(_) option
    member(Op, Operators)
  ; % Option III: it is part of the operators defined in the target
    Targets = [Target], %% TODO: Support more than one target, e.g.
                        %%   `targets([swi,gnu])`, to throw warnings
                        %%   for operators that are not defined in
                        %%   all target systems
    target_ops(Target, Target_Ops),
    member(Op, Target_Ops)
  ; % Option IV: operators should be inferred
    Inferred_Ops \== no,
    memberchk(Op, Inferred_Ops)
  ).

not_operator(Op0, Options) :-
  Op0 = op(Prec, Spec, Name0),
  % remove leading space if present
  ( atom_concat(' ', Name, Name0) ; Name = Name0 ), !,
  Op = op(Prec, Spec, Name),
  
  % get relevant options
  option(operators(Operators), Options),
  option(targets(Targets), Options),
  option(infer_operators(Inferred_Ops), Options),

  % check that it can not be defined as an operator
  \+ member(Op, Operators),
  ( Inferred_Ops == no ; not_member(Op, Inferred_Ops) ).


not_member(_, Ys) :-
  var(Ys), !.
not_member(_, []).
not_member(X, [Y|Ys]) :-
  X \= Y,
  not_member(X, Ys).

open_member(X, Xs) :-
  \+ var(Xs),
  Xs = [X|_].
open_member(X, [_|Xs]) :-
  \+ var(Xs),
  open_member(X, Xs).


principal_functor(term(_), indef).
principal_functor(term(Spec, [_, op(Atom_Tree), _]), Atom) :-
  member(Spec, [xfx, yfx, xfy]),
  atom_tree(Atom, Atom_Tree).
principal_functor(term(Spec, [_, op(Atom_Tree)]), Atom) :-
  member(Spec, [yf, xf]),
  atom_tree(Atom, Atom_Tree).
principal_functor(term(Spec, [op(Atom_Tree), _]), Atom) :-
  member(Spec, [fy, fx]),
  atom_tree(Atom, Atom_Tree).

spec_class( fx, prefix).
spec_class( fy, prefix).
spec_class(xfx, infix).
spec_class(xfy, infix).
spec_class(yfx, infix).
spec_class(xf , postfix).
spec_class(yf , postfix).

atom_tree(Atom, atom(name(L))) :-
  ( L = [name_token(Atom, _)]
  ; L = [_, name_token(Atom, _)] ).

atom_tree('[]', atom([open_list(_),close_list(_)])).
atom_tree('{}', atom([open_curly(_),close_curly(_)])).

get_operators(Opts, Term_Tree) :-
  ( Term_Tree = term(fx, [op(_), term(Term)]),
    Term = [atom(name(Name)), open_ct(_), arg_list(Arg_List0), close(_)],
    append(_, [name_token(Op, _)], Name) ->
    ( Op = op ->
      get_operator_from_term(Opts, term(Term))
    ; Op = module,
      Arg_List0 = [arg(_), comma(_), arg_list(arg(Arg1))],
      % [..., op(Prec,Spec,Functor), ... ]
      Arg1 = term([open_list(_), Items, close_list(_)]) ->
      get_operators_from_items(Opts, Items)
    ; otherwise ->
      true
    )
  ; otherwise ->
    true
  ).

get_operator_from_term(Opts, term(Term)) :-
  ( Term = [atom(name(Name)), open_ct(_), arg_list(Arg_List0), close(_)],
    append(_, [name_token(op, _)], Name),
    Arg_List0 = [arg(Arg0), comma(_), arg_list(Arg_List1)],
    Arg_List1 = [arg(Arg1), comma(_), arg_list(Arg_List2)],
    Arg_List2 = arg(Arg2),
    % Prec
    Arg0 = term(integer(Integer0)),
    append(_, [integer_token(Prec_Atom, _)], Integer0),
    atom_number(Prec_Atom, Prec),
    % Spec
    Arg1 = term(atom(name(Name1))),
    append(_, [name_token(Spec, _)], Name1),
    % Functor
    Arg2 = term(atom(name(Name2))),
    append(_, [name_token(Functor, _)], Name2)
  ->
    Op = op(Prec,Spec,Functor),
    option(specified_operators(Open_List), Opts, _),
    memberchk(Op, Open_List)
  ; otherwise ->
    true
  ).

get_operators_from_items(Opts, items([arg(Arg), comma(_), Items])) :-
  get_operator_from_term(Opts, Arg),
  get_operators_from_items(Opts, Items).
get_operators_from_items(Opts, items(arg(Arg))) :-
  get_operator_from_term(Opts, Arg).



/* 6.2 PROLOG TEXT AND DATA */

prolog(Opts, prolog(PT), A) :-
  \+ var(A), !,
  *(p_text(Opts), PT_PText, A, B),
  ( B = [],
    PT = PT_PText
  ; B = [layout_text_sequence(_)],
    append(PT_PText, B, PT) ).

prolog(Opts, prolog(PT), A) :-
  \+ var(PT), !,
  ( append(PT_PText, [layout_text_sequence(LTS)], PT),
    B = [layout_text_sequence(LTS)]
  ; B = [],
    PT = PT_PText ),
  *(p_text(Opts), PT_PText, A, B).

/*
prolog(Opts) -->
    *p_text(Opts).
*/
p_text(Opts, PT, A, C) :-
  \+ var(A), !,
  P #=< 1201,
  term(P, Opts, Term_Tree, A, B),
  end_(End_Tree, B, C),
  principal_functor(Term_Tree, Principal_Functor),
  (  Principal_Functor = (:-)
  -> Tree_Name = directive_term,
     get_operators(Opts, Term_Tree)
  ;  Tree_Name = clause_term ),
  PT =.. [Tree_Name, [Term_Tree, End_Tree]].

p_text(Opts, PT, A, C) :-
  \+ var(PT), !,
  PT =.. [Tree_Name, [Term_Tree, End_Tree]],
  term(P, Opts, Term_Tree, A, B),
  ( Tree_Name = directive_term ->
    get_operators(Opts, Term_Tree)
  ; otherwise ->
    true
  ),
  end_(End_Tree, B, C),
  P #=< 1201.

end_(end([end_token(end_char('.'))]), [name([name_token('.', graphic_token([graphic_token_char(graphic_char('.'))]))])|B], B).
end_(end([Layout_Text_Sequence,end_token(end_char('.'))]), [name([Layout_Text_Sequence,name_token('.', graphic_token([graphic_token_char(graphic_char('.'))]))])|B], B).


/* 6.3 TERMS */

:- discontiguous plammar:term/5.
:- discontiguous plammar:term_/5.

/* 6.3.1 Atomic terms */

/* 6.3.1.1 Numbers */

term_(0, _Opts) -->
    [ integer(_) ].

term_(0, _Opts) -->
    [ float_number(_) ].

/* 6.3.1.2 Negative numbers */

term_(0, _Opts) -->
    negative_sign_name
  , [ integer(_) ].

term_(0, _Opts) -->
    negative_sign_name
  , [ float_number(_) ].

%% TODO: "- 1" is currently accepted
%% "A term which is the name - followed directly by
%%    a numeric constant denotes the corresponding
%%    negative constant."

negative_sign_name(negative_sign_name(T), [T|Out], Out) :-
  T = name(L),
  Name_Token = name_token(
    graphic_token([
      graphic_token_char(
        graphic_char('-')
      )]
    )
  ),
  append(Pre, [Name_Token], L),
  ( Pre = []
  ; Pre = [Layout_Text_Sequence], is_whitespace(Layout_Text_Sequence)).

is_whitespace(A) :-
  A = layout_text_sequence([_]).

/* 6.3.1.3 Atoms */

term_(0, Opts, term_(Atom_Tree), In, Out) :-
  phrase(atom(Atom_Tree), In, Out),
  atom_tree(Atom, Atom_Tree),
  not_operator(op(_,_,Atom), Opts).

term_(P, Opts, term_(Atom_Tree), In, Out) :-
  phrase(atom(Atom_Tree), In, Out),
  atom_tree(Atom, Atom_Tree),
  is_operator(op(P,_,Atom), Opts).

atom -->
    [ name(_) ].

atom -->
    [ open_list(_) ]
  , [ close_list(_) ].

atom -->
    [ open_curly(_) ]
  , [ close_curly(_) ].

/* 6.3.2 Variables */

term_(0, _Opts) -->
    [ variable(_) ].

/* 6.3.3 Compund terms - functional notation */

term_(0, Opts) -->
    atom
  , [ open_ct(_) ]
  , arg_list(Opts)
  , [ close(_) ].

term_(0, Opts, term_(T), A, Z) :-
  option(allow_variable_name_as_functor(Allow), Opts),
  yes(Allow),
  T = [variable(VT), open_ct(H),L|N],
  A = [variable(VT), open_ct(H)|M],
  arg_list(Opts, L, M, [close(Q)|Z]),
  N = [close(Q)].

/*
arg_list(Opts) -->
    arg(Opts).

arg_list(Opts) -->
    arg(Opts)
  , [ comma(_) ]
  , arg_list(Opts).
*/

%% Optimised version:
arg_list(Opts, arg_list(Inner), A, Z) :-
  \+ var(A),
  arg(Opts, Arg, A, B),
  ( B = Z,
    Inner = Arg
  ; B = [comma(Comma)|C],
    arg_list(Opts, Arg_List, C, Z),
    Inner = [ Arg, comma(Comma), Arg_List ]
  ).
arg_list(Opts, arg_list(arg(Arg)), A, Z) :-
  \+ var(Arg),
  arg(Opts, arg(Arg), A, Z).
arg_list(Opts, arg_list([Arg, Comma, Arg_List]), A, Z) :-
  \+ var(Arg),
  arg(Opts, Arg, A, [Comma|B]),
  arg_list(Opts, Arg_List, B, Z).

/* 6.3.3.1 Arguments */

arg(Opts, arg(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_tree(Atom, Atom_Tree)
  , is_operator(op(_,_,Atom), Opts).

arg(Opts, arg(Term_Tree), In, Out) :-
    phrase(term(P, Opts, Term_Tree), In, Out)
  , is_priority(P)
  , P #=< 999.

/* 6.3.4 Compund terms - operator notation */

/* 6.3.4.1 Operand */

%% Note that we do not distinguish between
%% `term` and `lterm` to avoid trivial
%% non-termination because of left-recursion
/*
term_(0, Opts) -->
    [ open(_) ]
  , term(P, Opts)
  , [ close(_) ]
  , { P #=< 1201 }.

term_(0, Opts) -->
    [ open_ct(_) ]
  , term(P, Opts)
  , [ close(_) ]
  , { P #=< 1201 }.
*/
term_(0, Opts, term_(Inner), A, Z) :-
  Inner = [open(Open_Tree), Term_Tree, close(Close_Tree)],
  A = [open(Open_Tree)|B],
  term(P, Opts, Term_Tree, B, C),
  C = [close(Close_Tree)|Z],
  P #=< 1201.

term_(0, Opts, term_(Inner), A, Z) :-
  Inner = [open_ct(Open_Ct_Tree), Term_Tree, close(Close_Tree)],
  A = [open_ct(Open_Ct_Tree)|B],
  term(P, Opts, Term_Tree, B, C),
  C = [close(Close_Tree)|Z],
  P #=< 1201.

/* 6.3.4.2 Operators as functors */

:- op(300, xfx, '@').

term(P, Opts, Res, A, Z) :-
  \+ var(A),
  term_(Term1_P, Opts, term_(Term1_Tree_), A, B),
  lterm(Opts, term(Term1_Tree_)@Term1_P, Res@P, B, Z).

term(P, Opts, Res, A, Z) :-
  \+ var(A),
  op(Op_P, Type, Opts, Op_Tree, A, B),
  spec_class(Type, prefix),
  term(P_Term, Opts, Term_Tree, B, C),
  lterm(Opts, term(Type, [Op_Tree, Term_Tree])@Op_P, Res@P, C, Z),
  prec_constraints(Type, Op_P, P_Term).

lterm(Opts, Term_Tree@P, Term_Tree@P, A, A).

lterm(Opts, Term1_Tree@Term1_P, Res@P, A, Z) :-
  op(Op_P, Type, Opts, Op_Tree, A, B),
  ( spec_class(Type, infix),
    term(Term2_P, Opts, Term2_Tree, B, C),
    lterm(Opts, term(Type, [Term1_Tree, Op_Tree, Term2_Tree])@Op_P, Res@P, C, Z),
    prec_constraints(Type, Op_P, Term1_P, Term2_P)
  ; spec_class(Type, postfix),
    lterm(Opts, term(Type, [Term1_Tree, Op_Tree])@Op_P, Res@P, B, Z),
    prec_constraints(Type, Op_P, Term1_P) ).

term(0, Opts, Res, A, Z) :-
  \+ var(Res),
  ( Res = term(Inner),
    term_(_, Opts, term_(Inner), A, Z)
  ; Res = term(Type, [Term_Tree1, Op_Tree, Term_Tree2]),
    term(P_Term1, Opts, Term_Tree1, A, B),
    op(P_Op, Type, Opts, Op_Tree, B, C),
    term(P_Term2, Opts, Term_Tree2, C, Z)
  ; Res = term(Type, [Term_Tree, Op_Tree]),
    member(Type, [xf, yf]),
    term(P_Term, Opts, Term_Tree, A, B),
    op(P_Op, Type, Opts, Op_Tree, B, Z)
  ; Res = term(Type, [Op_Tree, Term_Tree]),
    member(Type, [fx, fy]),
    op(P_Op, Type, Opts, Op_Tree, A, B),
    term(P_Term, Opts, Term_Tree, B, Z)
  ).

prec_constraints(xfx, P_Op, P_Term1, P_Term2) :-
  P_Term1 #< P_Op,
  P_Term2 #< P_Op.
prec_constraints(yfx, P_Op, P_Term1, P_Term2) :-
  P_Term1 #=< P_Op,
  P_Term2 #< P_Op.
prec_constraints(xfy, P_Op, P_Term1, P_Term2) :-
  P_Term1 #< P_Op,
  P_Term2 #=< P_Op.
prec_constraints(xf, P_Op, P_Term) :-
  P_Term #< P_Op.
prec_constraints(yf, P_Op, P_Term) :-
  P_Term #=< P_Op.
prec_constraints(fx, P_Op, P_Term) :-
  P_Term #< P_Op.
prec_constraints(fy, P_Op, P_Term) :-
  P_Term #=< P_Op.

/* 6.3.4.3 Operators */

op(P, Spec, Opts, op(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_tree(Atom, Atom_Tree)
  , is_operator(op(P, Spec, Atom), Opts).

op(1000, xfy, _Opts) -->
    [ comma(_) ].

/* 6.3.5 Compound terms - list notation */

term_(0, Opts) -->
    [ open_list(_) ]
  , items(Opts)
  , [ close_list(_) ].

% .(h,l)
items(Opts) -->
    arg(Opts)
  , [ comma(_) ]
  , items(Opts).

% .(h,t)
items(Opts) -->
    arg(Opts)
  , [ ht_sep(_) ]
  , arg(Opts).

% .(t, [])
items(Opts) -->
    arg(Opts).

/* 6.3.6 Compound terms - curly bracketed term */

% {}(l)
term_(0, Opts) -->
    [ open_curly(_) ]
  , term(P, Opts)
  , [ close_curly(_) ]
  , { P #=< 1201 }.

/* 6.3.7 Terms - double quoted list notation */

term_(0, _Opts) -->
    [ double_quoted_list(_) ].

/* Extension for SWI 7+ - back quoted string notation */
term_(0, _Opts) -->
    [ back_quoted_string(_) ].
