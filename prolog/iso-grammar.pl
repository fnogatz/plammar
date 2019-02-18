%%%%
%%%% THIS IS JUST LEGACY CODE
%%%%
%%%% PLEASE IGNORE
%%%%


:- discontiguous
  plammar:term/5,
  plammar:lterm/5.

:- use_module(library(clpfd)).

is_priority(P) :-
  P #>= 0,
  P #=< 1201.

is_operator(Atom, ops(Table, Nots)) :-
  is_operator(Atom, ops(Table, Nots), _).

is_operator(Atom, Ops, Op) :-
  atom_concat(' ', Without_Space, Atom),
  !,
  is_operator(Without_Space, Ops, Op).

is_operator(Atom, ops(Table, Nots), Op) :-
  Op = op(_, _, Atom),
  not_member(Op, Nots),
  memberchk(Op, Table).

not_operator(Atom, Ops) :-
  atom_concat(' ', Without_Space, Atom),
  !,
  not_operator(Without_Space, Ops).

not_operator(Atom, ops(Table, Nots)) :-
  Op = op(_, _, Atom),
  not_member(Op, Table),
  memberchk(Op, Nots).

not_member(_, Ys) :-
  var(Ys), !.
not_member(_, []).
not_member(X, [Y|Ys]) :-
  X \= Y,
  not_member(X, Ys).

%% prolog//0 is the entry point for the library's
%%   interfaces.
prolog -->
    read_term_.


/* 6.2 PROLOG TEXT AND DATA */

/* 6.2.1 Prolog text */

prolog_text -->
    p_text.

p_text -->
    directive_term
  , p_text.

p_text -->
    clause_term
  , p_text.

p_text -->
    [].

/* 6.2.1.1 Directives */

directive_term -->
    term
  , end.
%% TODO: Condition "The principal functor of dt is (:-)/1"

directive -->
    directive_term.

/* 6.2.1.2 Clauses */

clause_term -->
    term
  , end.
%% TODO: Condition "The principal functor of c is not (:-)/1"

/* 6.2.2 Prolog data */

read_term_ -->
    term
  , end.


/* 6.3 TERMS */

/* 6.3.1 Atomic terms */

/* 6.3.1.1 Numbers */

term(0, _Ops) -->
    integer
  , !.

term(0, _Ops) -->
    float_number
  , !.

/* 6.3.1.2 Negative numbers */

term(0, _Ops) -->
    negative_sign_char
  , integer
  , !.

term(0, _Ops) -->
    negative_sign_char
  , float_number
  , !.

/* 6.3.1.3 Atoms */

term(P, Ops, term(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , !
  , atom_chars(Atom, In)
  , ( not_operator(Atom, Ops), P = 0
    ; is_operator(Atom, Ops), P = 1201).
/*
term(0, Ops, term(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_chars(Atom, In)
  , not_operator(Atom, Ops).

term(1201, Ops, term(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_chars(Atom, In)
  , is_operator(Atom, Ops).
*/
atom -->
    name.

atom -->
    open_list
  , close_list.

atom -->
    open_curly
  , close_curly.

/* 6.3.2 Variables */

term(0, _Ops) -->
    variable
  , !.

/* 6.3.3 Compund terms - functional notation */
/*
term(0, Ops) -->
    atom
  , open_ct
  , arg_list(Ops)
  , close_.
*/
arg_list(Ops) -->
    arg_(Ops).

arg_list(Ops) -->
    arg_(Ops)
  , comma
  , arg_list.

/* 6.3.3.1 Arguments */

arg(Ops, arg(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_chars(Atom, In)
  , is_operator(Atom, Ops).

arg(Ops, arg(Term_Tree), In, Out) :-
    phrase(term(P, Ops, Term_Tree), In, Out)
  , is_priority(P)
  , P #=< 999.

/* 6.3.4 Compund terms - operator notation */

%% TODO
/*
term(0, Ops, term([Open_Tree, Term_Tree, Close_Tree]), In, Out) :-
  phrase(open(Open_Tree), In, A),
  phrase(term(P, Ops, Term_Tree), A, B),
  phrase(close(Close_Tree), B, Out).
*/
/*
Ops = ops(Table, Nots), phrase(ast:term(Prio, Ops, Res), ['f', '(', '(', 'a', ')', ')'], []), nl, print_term(Res, [indent_arguments(1)]).
*/

/* 6.3.4.1 Operand */

% term = lterm
term(P, Ops, LTerm_Tree, In, Out) :-
writeln(ok-1),
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #=< P
%,writeln(ok1)
%,get_attr(P, clpfd, A), writeln(A)
  , lterm(P_Tree, Ops, LTerm_Tree, In, Out).

% lterm = term
lterm(P, Ops, Term_Tree, In, Out) :-
writeln(ok-2),
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #=< P-1
%,writeln(ok2)
%,get_attr(P, clpfd, A), writeln(A)
  , term(P_Tree, Ops, Term_Tree, In, Out).

% term = open, term, close
term(0, Ops) -->
    open_
  , { is_priority(P) }
  , term(P, Ops)
  , close_
  , !.

% term = open ct, term, close
term(0, Ops) -->
    open_ct
  , { is_priority(P) }
  , term(P, Ops)
  , close_
  , !.

/* 6.3.4.2 Operators as functors */

%% TODO

% lterm = op, term
lterm(P, Ops, lterm([Op_Tree, Term_Tree]), In, Out) :-
writeln(ok-3),
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #< P
  , phrase(op(P, fx, Ops, Op_Tree), In, Rest1)
  , phrase(term(P_Tree, Ops, Term_Tree), Rest1, Out).

% term = op, term
term(P, Ops, term([Op_Tree, Term_Tree]), In, Out) :-
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #=< P
  , phrase(op(P, fy, Ops, Op_Tree), In, Rest1)
  , phrase(term(P_Tree, Ops, Term_Tree), Rest1, Out).

%% TODO: Avoid left-recursion
% lterm = lterm, op
lterm(P, Ops, lterm([Term_Tree, Op_Tree]), In, Out) :-
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #=< P
  , phrase(lterm(P_Tree, Ops, Term_Tree), In, Rest1)
  , phrase(op(P, yf, Ops, Op_Tree), Rest1, Out).

% lterm = term, op
lterm(P, Ops, lterm([Term_Tree, Op_Tree]), In, Out) :-
    is_priority(P)
  , is_priority(P_Tree)
  , P_Tree #< P
  , phrase(term(P_Tree, Ops, Term_Tree), In, Rest1)
  , phrase(op(P, xf, Ops, Op_Tree), Rest1, Out).

/* 6.3.4.3 Operators */

op(P, Spec, Ops, op(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , append(Consumed, Out, In)
  , atom_chars(Atom, Consumed)
  , is_operator(Atom, Ops, Op)
  , Op = op(P, Spec, _).

op(1000, xfy, _Ops) -->
    comma.

