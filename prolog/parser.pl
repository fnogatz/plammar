:- module(parser, []).

is_priority(P) :-
  P #>= 0,
  P #=< 1201.

is_operator(Op, Table) :-
  Op = op(Prec, Spec, Name),
  atom_concat(' ', Without_Space, Name),
  !,
  is_operator(op(Prec, Spec, Without_Space), Table).

is_operator(Op, ops(Ops, Nots)) :-
  Op = op(Prec, Spec, Name),
  not_member(Op, Nots),
  memberchk(Op, Ops),
  is_priority(Prec),
  %% 6.3.4.3
  %%   "There shall not be two Operators with the same class and name."
  spec_class(Spec, Class),
  \+((
    Op2 = op(_, Spec2, Name),   % another operator
    memberchk(Op2, Ops),        % with same name
    spec_class(Spec2, Class),   % with same class
    Spec \= Spec2               % but different spec
  )).

not_operator(Op, Table) :-
  Op = op(Prec, Spec, Name),
  atom_concat(' ', Without_Space, Name),
  !,
  not_operator(op(Prec, Spec, Without_Space), Table).

not_operator(Op, ops(Ops, Nots)) :-
  not_member(Op, Ops),
  memberchk(Op, Nots).

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

atom_tree(Atom, Tree) :-
  remove_whitespaces(Tree, atom(Tree_Wo_Whitespace)),
  atom_tree_(Atom, Tree_Wo_Whitespace).

atom_tree_('[]', EmptyList) :-
  EmptyList = [open_list([open_list_token(open_list_char('['))]),close_list([close_list_token(close_list_char(']'))])].
atom_tree_('{}', EmptyCurly) :-
  EmptyCurly = [open_curly([open_curly_token(open_curly_char('{'))]),close_curly([close_curly_token(close_curly_char('}'))])].
atom_tree_(Atom, name(Name_Tree)) :-
  phrase(name(name(Name_Tree)), Chars, []),
  atom_chars(Atom, Chars).

remove_whitespaces(Term, Term_) :-
  \+is_list(Term),
  Term =.. [F|TermL],
  remove_whitespaces(TermL, TermL_),
  Term_ =.. [F|TermL_].

remove_whitespaces([], []).
remove_whitespaces([layout_text_sequence(_)|Xs], Xs_) :-
  !,
  remove_whitespaces(Xs, Xs_).
remove_whitespaces([X|Xs], [X_|Xs_]) :-
  remove_whitespaces(X, X_),
  remove_whitespaces(Xs, Xs_).


/* 6.3 TERMS */

:- discontiguous plammar:term/5.

/* 6.3.1 Atomic terms */

/* 6.3.1.1 Numbers */

term(0, _Ops) -->
    [ integer(_) ].

term(0, _Ops) -->
    [ float_number(_) ].

/* 6.3.1.2 Negative numbers */

term(0, _Ops) -->
    negative_sign_name
  , [ integer(_) ].

term(0, _Ops) -->
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

term(0, Ops, term(Atom_Tree), In, Out) :-
  phrase(atom(Atom_Tree), In, Out),
  atom_tree(Atom, Atom_Tree),
  not_operator(op(_,_,Atom), Ops).

term(P, Ops, term(Atom_Tree), In, Out) :-
  phrase(atom(Atom_Tree), In, Out),
  atom_tree(Atom, Atom_Tree),
  is_operator(op(P,_,Atom), Ops).

atom -->
    [ name(_) ].

atom -->
    [ open_list(_) ]
  , [ close_list(_) ].

atom -->
    [ open_curly(_) ]
  , [ close_curly(_) ].

/* 6.3.2 Variables */

term(0, _Ops) -->
    [ variable(_) ].

/* 6.3.3 Compund terms - functional notation */

term(0, Ops) -->
    atom
  , [ open_ct(_) ]
  , arg_list(Ops)
  , [ close_(_) ].

arg_list(Ops) -->
    arg(Ops).

arg_list(Ops) -->
    arg(Ops)
  , [ comma(_) ]
  , arg_list(Ops).

/* 6.3.3.1 Arguments */

arg(Ops, arg(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_tree(Atom, Atom_Tree)
  , is_operator(op(_,_,Atom), Ops).

arg(Ops, arg(Term_Tree), In, Out) :-
    phrase(term(P, Ops, Term_Tree), In, Out)
  , is_priority(P)
  , P #=< 999.

/* 6.3.4 Compund terms - operator notation */

/* 6.3.4.1 Operand */

%% Note that we do not distinguish between
%% `term` and `lterm` to avoid trivial
%% non-termination because of left-recursion

term(0, Ops) -->
    [ open_(_) ]
  , term(P, Ops)
  , [ close_(_) ]
  , { P #=< 1201 }.

term(0, Ops) -->
    [ open_ct(_) ]
  , term(P, Ops)
  , [ close_(_) ]
  , { P #=< 1201 }.

/* 6.3.4.2 Operators as functors */

% term = term, op, term (xfx)
term(P, Ops, term(xfx, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #< P
  , P_Term2 #< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, xfx, Ops, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Ops, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Ops, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op, term (yfx)
term(P, Ops, term(yfx, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #=< P
  , P_Term2 #< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, yfx, Ops, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Ops, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Ops, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op, term (xfy)
term(P, Ops, term(xfy, [Term1_Tree, Op_Tree, Term2_Tree]), In, Out) :-
    P_Term1 #< P
  , P_Term2 #=< P
    % define instructions
  , I0 = append(Term_Part, Out, In)
  , I1 = append(Term1, [Op|Term2], Term_Part)
  , I2 = phrase(op(P, xfy, Ops, Op_Tree), [Op])
  , I3 = phrase(term(P_Term1, Ops, Term1_Tree), Term1)
  , I4 = phrase(term(P_Term2, Ops, Term2_Tree), Term2)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2, I3, I4)
    ; \+ var(Term1_Tree) -> Instructions = (I3, I2, I4, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op (yf)
term(P, Ops, term(yf, [Term_Tree, Op_Tree]), In, Out) :-
    P_Term #=< P
    % define instructions
  , I0 = append(Term_Part, [Op | Out], In)
  , I1 = phrase(op(P, yf, Ops, Op_Tree), [Op])
  , I2 = phrase(term(P_Term, Ops, Term_Tree), Term_Part)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2)
    ; \+ var(Term_Tree) -> Instructions = (I2, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = term, op (xf)
term(P, Ops, term(xf, [Term_Tree, Op_Tree]), In, Out) :-
    P_Term #< P
    % define instructions
  , I0 = append(Term_Part, [Op | Out], In)
  , I1 = phrase(op(P, xf, Ops, Op_Tree), [Op])
  , I2 = phrase(term(P_Term, Ops, Term_Tree), Term_Part)
    % call instructions in best order
  , ( \+ var(In) -> Instructions = (I0, I1, I2)
    ; \+ var(Term_Tree) -> Instructions = (I2, I1, I0)
    ; print_message(warning, format('Error while handling 6.3.4.2.')) )
  , call(Instructions).

% term = op, term (fy)
term(P, Ops, term(fy, [Op_Tree, Term_Tree]), [Op|Rest], Out) :-
    P_Term #=< P
  , phrase(op(P, fy, Ops, Op_Tree), [Op])
  , phrase(term(P_Term, Ops, Term_Tree), Rest, Out).

% term = op, term (fx)
term(P, Ops, term(fx, [Op_Tree, Term_Tree]), [Op|Rest], Out) :-
    P_Term #< P
  , phrase(op(P, fx, Ops, Op_Tree), [Op])
  , phrase(term(P_Term, Ops, Term_Tree), Rest, Out).

/* 6.3.4.3 Operators */

op(P, Spec, Ops, op(Atom_Tree), In, Out) :-
    phrase(atom(Atom_Tree), In, Out)
  , atom_tree(Atom, Atom_Tree)
  , is_operator(op(P, Spec, Atom), Ops).

op(1000, xfy, _Ops) -->
    [ comma(_) ].

/* 6.3.5 Compund terms - list notation */

term(0, Ops) -->
    [ open_list(_) ]
  , items(Ops)
  , [ close_list(_) ].

% .(h,l)
items(Ops) -->
    arg(Ops)
  , [ comma(_) ]
  , items(Ops).

% .(h,t)
items(Ops) -->
    arg(Ops)
  , [ ht_sep(_) ]
  , arg(Ops).

% .(t, [])
items(Ops) -->
    arg(Ops).
