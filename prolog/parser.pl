:- module(parser, []).

is_priority(P) :-
  P #>= 0,
  P #=< 1201.

is_operator(Op0, Options) :-
  Op0 = op(Prec, Spec, Name0),
  % remove leading space if present
  ( atom_concat(' ', Name, Name0) ; Name = Name0 ), !,
  Op = op(Prec, Spec, Name),
  is_priority(Prec),
  option(operators(Operators), Options),
  option(infer_operators(Inferred_Ops), Options),
  ( member(Op, Operators)
  ; Inferred_Ops == no,
    !, false
  ; option(infer_operators(Inferred_Ops), Options),
    memberchk(Op, Inferred_Ops)
  ).

not_operator(Op0, Options) :-
  Op0 = op(Prec, Spec, Name0),
  % remove leading space if present
  ( atom_concat(' ', Name, Name0) ; Name = Name0 ), !,
  Op = op(Prec, Spec, Name),
  option(operators(Operators), Options),
  \+ member(Op, Operators),
  ( option(infer_operators(no), Options)
  ; option(infer_operators(Inferred_Ops), Options),
    not_member(Op, Inferred_Ops)
  ).

not_member(_, Ys) :-
  var(Ys), !.
not_member(_, []).
not_member(X, [Y|Ys]) :-
  X \= Y,
  not_member(X, Ys).

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


/* 6.3 TERMS */

:- discontiguous plammar:term/5.

/* 6.3.1 Atomic terms */

/* 6.3.1.1 Numbers */

term(0, _Opts) -->
    [ integer(_) ].

term(0, _Opts) -->
    [ float_number(_) ].

/* 6.3.1.2 Negative numbers */

term(0, _Opts) -->
    negative_sign_name
  , [ integer(_) ].

term(0, _Opts) -->
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

term(0, Opts, term(Atom_Tree), In, Out) :-
  phrase(atom(Atom_Tree), In, Out),
  atom_tree(Atom, Atom_Tree),
  not_operator(op(_,_,Atom), Opts).

term(P, Opts, term(Atom_Tree), In, Out) :-
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

term(0, _Opts) -->
    [ variable(_) ].

/* 6.3.3 Compund terms - functional notation */

term(0, Opts) -->
    atom
  , [ open_ct(_) ]
  , arg_list(Opts)
  , [ close_(_) ].

arg_list(Opts) -->
    arg(Opts).

arg_list(Opts) -->
    arg(Opts)
  , [ comma(_) ]
  , arg_list(Opts).

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

term(0, Opts) -->
    [ open_(_) ]
  , term(P, Opts)
  , [ close_(_) ]
  , { P #=< 1201 }.

term(0, Opts) -->
    [ open_ct(_) ]
  , term(P, Opts)
  , [ close_(_) ]
  , { P #=< 1201 }.

/* 6.3.4.2 Operators as functors */

% term = term, op, term (xfx)
term(P, Opts, term(xfx, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #< P
  , P_Term2 #< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, xfx, Opts, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Opts, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Opts, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op, term (yfx)
term(P, Opts, term(yfx, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #=< P
  , P_Term2 #< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, yfx, Opts, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Opts, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Opts, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op, term (xfy)
term(P, Opts, term(xfy, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #< P
  , P_Term2 #=< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, xfy, Opts, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Opts, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Opts, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op (yf)
term(P, Opts, term(yf, [Term_Tree, Op_Tree]), In, Out) :-
    P_Term #=< P
    % define instructions
  , I0 = append(Term_Part, [Op | Out], In)
  , I1 = phrase(op(P, yf, Opts, Op_Tree), [Op])
  , I2 = phrase(term(P_Term, Opts, Term_Tree), Term_Part)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2)
    ; \+ var(Term_Tree) -> Instructions = (I2, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op (xf)
term(P, Opts, term(xf, [Term_Tree, Op_Tree]), In, Out) :-
    P_Term #< P
    % define instructions
  , I0 = append(Term_Part, [Op | Out], In)
  , I1 = phrase(op(P, xf, Opts, Op_Tree), [Op])
  , I2 = phrase(term(P_Term, Opts, Term_Tree), Term_Part)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2)
    ; \+ var(Term_Tree) -> Instructions = (I2, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = op, term (fy)
term(P, Opts, term(fy, [Op_Tree, Term_Tree]), [Op|Rest], Out) :-
    P_Term #=< P
  , phrase(op(P, fy, Opts, Op_Tree), [Op])
  , phrase(term(P_Term, Opts, Term_Tree), Rest, Out).

% term = op, term (fx)
term(P, Opts, term(fx, [Op_Tree, Term_Tree]), [Op|Rest], Out) :-
    P_Term #< P
  , phrase(op(P, fx, Opts, Op_Tree), [Op])
  , phrase(term(P_Term, Opts, Term_Tree), Rest, Out).

/* 6.3.4.3 Operators */

op(P, Spec, Opts, op(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_tree(Atom, Atom_Tree)
  , is_operator(op(P, Spec, Atom), Opts).

op(1000, xfy, _Opts) -->
    [ comma(_) ].

/* 6.3.5 Compound terms - list notation */

term(0, Opts) -->
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
term(0, Opts) -->
    [ open_curly(_) ]
  , term(P, Opts)
  , [ close_curly(_) ]
  , { P #=< 1201 }.

/* 6.3.7 Terms - double quoted list notation */

term(0, _Opts) -->
    [ double_quoted_list(_) ].
