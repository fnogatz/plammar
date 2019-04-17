:- use_module(library(yall)).

%% Part I: string given, parse tree unbound 

'"a" is not a valid Prolog program' :-
  % missing end token after clause
  \+ prolog_parsetree(string("a"), _).

'"a." has a single parse tree' :-
  findall(
    PT,
    prolog_parsetree(string("a."), PT),
    PTs
  ),
  PTs = [
    prolog([clause_term([term(atom(name([name_token('a', letter_digit_token([small_letter_char(a)]))]))),end([end_token(end_char('.'))])])])
  ].

'"a. b. c." has a single parse tree' :-
  findall(
    PT,
    prolog_parsetree(string("a. b. c."), PT),
    PTs
  ),
  PTs = [
    prolog([
      clause_term([term(atom(name([name_token('a', letter_digit_token([small_letter_char(a)]))]))),
        end([end_token(end_char(.))])]),
      clause_term([
        term(atom(name([
          layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token('b', letter_digit_token([small_letter_char(b)]))]))),
        end([end_token(end_char(.))])]),
      clause_term([
        term(atom(name([
          layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token('c', letter_digit_token([small_letter_char(c)]))]))),
        end([end_token(end_char(.))])])])
  ].

'"a :- b." has a single parse tree' :-
  findall(
    PT,
    prolog_parsetree(string("a :- b."), PT),
    PTs
  ),
  PTs = [
    prolog([directive_term([
      term(xfx,[
        term(atom(name([
          name_token('a', letter_digit_token([small_letter_char(a)]))]))),
        op(atom(name([
          layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token(':-', graphic_token([graphic_token_char(graphic_char(':')),graphic_token_char(graphic_char('-'))]))]))),
        term(atom(name([
          layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token('b', letter_digit_token([small_letter_char(b)]))])))]),
      end([end_token(end_char('.'))])])
    ])
  ].

'"a :- b." cannot be parsed when ISO operators are not used'(fail) :-
  prolog_parsetree(string("a :- b."), _PT, [ targets([]) ]).

'"a b c." can be parsed with the appropriate operator' :-
  % try all variants xfx, xfy, and yfx
  maplist([Type]>>(
    prolog_parsetrees(string("a b c."), PTs, [ operators([ op(800,Type,b) ]) ]),
    length(PTs, 1)
  ), [xfx, xfy, yfx]).

'"f :- a." is valid for f not being an operator' :-
  Options = [],
  prolog_parsetrees(string("f :- a."), PTs, Options),
  length(PTs, 1).

'"f :- a." is invalid for f being an operator fy@1100'(fail) :-
  Options = [ operators([ op(1100,fy,f) ]) ],
  prolog_parsetree(string("f :- a."), _PTs, Options).

'"f :- a." is invalid for f being an operator fx@1100'(fail) :-
  Options = [ operators([ op(1100,fx,f) ]) ],
  prolog_parsetree(string("f :- a."), _PT, Options).

'"f :- a." is valid for f being an operator fy@1200' :-
  Options = [ operators([ op(1200,fy,f) ]) ],
  prolog_parsetrees(string("f :- a."), PTs, Options),
  length(PTs, 1).

'"f :- a." is not valid for f being an operator fx@1200' :-
  Options = [ operators([ op(1200,fx,f) ]) ],
  prolog_parsetrees(string("f :- a."), PTs, Options),
  length(PTs, 0).


%% Part II: parse tree given, string unbound

'Get "a." from parse tree' :-
  PT = prolog([clause_term([term(atom(name([name_token('a', letter_digit_token([small_letter_char(a)]))]))),end([end_token(end_char('.'))])])]),
  findall(
    String,
    prolog_parsetree(string(String), PT),
    Strings
  ),
  Strings = ["a."].

%% Part III: automatic deterministic conversion string->parsetree->string

%%% III.a): Example Prolog programs

prolog("a.").
prolog("a. b.").

prolog("a(b).").
prolog("a(1).").
prolog("a(1.2).").
prolog("a(X).").
prolog("a(_).").
prolog("a(1,2).").
prolog("a( 1 , 2 ).").

prolog("a(b(c)).").

prolog("a(1,2,3,4,5,6,7,8,9,10).").

prolog(":- a.").
prolog(":- a, b.").
prolog(":- [load].").

prolog("a :- b.").

% Ends with layout_text_sequence

prolog("a.\n").
prolog("a. % this is a comment\n").
prolog("a.% this is a comment\n").
prolog("a. /* this is a comment */\n").

% Comments

prolog("a. % linecomment\nb.").
prolog("a.% linecomment\nb.").
prolog("a.%a%b%c\nb.").
prolog("a. /**/\nb.").
prolog("a. /* this is a comment */\nb.").
prolog("a. /*/**/\nb."). % no nesting of comments
% missing layout_char between end_token and next clause:
invalid("a./**/\nb.").
invalid("a./* this is a comment */\nb.").
% line comment ends with end_of_file
prolog("a.% linecomment").

% Sec. 6.3.5, Compound terms - list notation
prolog("list([]).").
prolog("list([X]).").
prolog("list([a,1]).").
prolog("list( [ b , 2 ] ).").

prolog("member(X,[X|_]).").
prolog("member(X,[_|Xs]) :- member(X,Xs).").

prolog("list([1,2,3,4,5,6,7,8,9,10]).").

% Sec. 6.3.6, Compound terms - curly bracketed term
prolog("curly({}).").
prolog("curly({a}).").
prolog("curly({a,1}).").
prolog("curly( { b , 2 } ).").

% Sec. 6.3.7, Terms - double quoted list notation
prolog("a(\"\").").
prolog("a(\"s\").").
prolog("a(\"string with blanks\").").

% Back quoted text, as in SWI 7+
prolog("a(`b`).", [ back_quoted_text(yes) ]).
prolog("a(`b`).", [ targets([swi(7)]) ]).

%%% III.b): Operators

'"a(1+1)." is correctly parsed' :-
  prolog_parsetrees(string("a(1+1)."), PTs, []),
  PTs = [
    prolog([
      clause_term([
        term([
          atom(name([name_token(a,letter_digit_token([small_letter_char(a)]))])),
          open_ct(
            open_token(open_char('('))),
            arg_list(arg(term(yfx,[
              term(integer([integer_token('1',integer_constant([decimal_digit_char('1')]))])),
              op(atom(name([name_token(+,graphic_token([graphic_token_char(graphic_char(+))]))]))),
              term(integer([integer_token('1',integer_constant([decimal_digit_char('1')]))]))]))),
            close([close_token(close_char(')'))])]),
        end([end_token(end_char('.'))])
      ])
    ])
  ].

prolog("a((1+2)+3).").
prolog("a(1+(2+3)).").

% left-associative
prolog("a(1+2+3).", [ targets([]), operators([ op(500,xfy,+) ]) ]).
prolog("a(1+2+3+4).", [ targets([]), operators([ op(500,xfy,+) ]) ]).

% right-associative
prolog("a(2+4+6).", [ targets([]), operators([ op(500,yfx,+) ]) ]).

% mixed
prolog("p(1 a 2 b 3 c 4).", [ operators([ op(200, xfx, b), op(300, xfx, a), op(400, xfx, c) ]) ]).

prolog("a b c.", [ operators([ op(600, xfx, b) ]) ]).
prolog("a b c.", [ operators([ op(600, yfx, b) ]) ]).
prolog("a b c.", [ operators([ op(600, xfy, b) ]) ]).

% Sec. 6.3, Table 5, valid terms
prolog("fx (fx 1).", [ operators([ op(600, fx, fx) ]) ]).
prolog("(1 xf) xf.", [ operators([ op(600, xf, xf) ]) ]).
prolog("(1 xfx 2) xfx 3.", [ operators([ op(600, xfx, xfx) ]) ]).
prolog("1 xfx (2 xfx 3).", [ operators([ op(600, xfx, xfx) ]) ]).

% Sec. 6.3, Table 5, invalid terms
invalid("fx fx 1.", [ operators([ op(600, fx, fx) ]) ]).
invalid("1 xf xf.", [ operators([ op(600, xf, xf) ]) ]).
invalid("1 xfx 2 xfx 3.", [ operators([ op(600, xfx, xfx) ]) ]).

%%% III.c): Complex Example Prolog programs

prolog("a :- b, d.", [ targets([swi]) ]).
prolog("a :- b:c.", [ targets([swi]) ]).
prolog("a :- b:c, d.", [ targets([swi]) ]).

prolog("a :- fx q, c.", [ operators([ op(600, fx, fx) ]) ]).
prolog("a :- fy q, c.", [ operators([ op(600, fy, fy) ]) ]).
prolog("a :- q xf, c.", [ operators([ op(600, xf, xf) ]) ]).
prolog("a :- q yf, c.", [ operators([ op(600, yf, yf) ]) ]).

%% Part IV: handling of options

'Explicit statement of pre-defined ISO operators is allowed' :-
  Options = [
    operators([ op(1200, xfx, ':-') ]),
    targets([iso])
  ],
  prolog_parsetree(string("a."), _, Options), !.

'"A :- B." is valid for var_prefix(true)' :-
  Options = [
    var_prefix(true)
  ],
  prolog_parsetrees(string("A :- B."), PTs, Options),
  !,
  PTs = [ _SingleResult ].

'"A B." is valid for var_prefix(true) and A as prefix operator' :-
  Options = [
    var_prefix(true),
    operators([ op(300, fx, 'A') ])
  ],
  prolog_parsetrees(string("A B."), PTs, Options),
  !,
  PTs = [ _SingleResult ].

'"F(1)." is invalid for allow_variable_name_as_functor(no)' :-
  Options = [
    allow_variable_name_as_functor(no)
  ],
  prolog_parsetrees(string("F(1)."), PTs, Options),
  !,
  PTs = [].

'"F(1)." is valid for allow_variable_name_as_functor(true)' :-
  Options = [
    allow_variable_name_as_functor(true)
  ],
  prolog_parsetrees(string("F(1)."), PTs, Options),
  !,
  PTs = [ _SingleResult ].

'"_x(1)." is valid for allow_variable_name_as_functor(true)' :-
  Options = [
    allow_variable_name_as_functor(true)
  ],
  prolog_parsetrees(string("_x(1)."), PTs, Options),
  !,
  PTs = [ _SingleResult ].

'"_(1)." is valid for allow_variable_name_as_functor(true)' :-
  Options = [
    allow_variable_name_as_functor(true)
  ],
  prolog_parsetrees(string("_(1)."), PTs, Options),
  !,
  PTs = [ _SingleResult ].

invalid("A(B(C)).", [allow_variable_name_as_functor(false)] ).
prolog("A(B(C)).", [allow_variable_name_as_functor(true)] ).

%% Part V: infer operator definitions

'"a b." invalid for infer_operators(no)' :-
  Options = [ targets([]), infer_operators(no) ],
  \+ prolog_parsetree(string("a b."), _, Options).

'"a b." valid for infer_operators(yes)' :-
  Options = [ targets([]), infer_operators(yes) ],
  prolog_parsetrees(string("a b."), PTs, Options),
  length(PTs, PTs_Count),
  % with the current implementation, it is 4, because of:
  % a@fx, b not an operator
  % a@fy, b not an operator
  % b@xf, a not an operator
  % b@yf, a not an operator
  PTs_Count = 4.

'"a b." valid for infer_operators(Ops)' :-
  findall(
    Ops,
    prolog_parsetree(
      string("a b."),
      PT,
      [ targets([]), infer_operators(Ops) ]
    ),
    Inferred_Ops
  ),
  Expected = [
    [op(_,yf,b)],
    [op(_,xf,b)],
    [op(_,fy,a)],
    [op(_,fx,a)]
  ],
  permutation2(Inferred_Ops, Expected), !.

%% Part VI: use operator definitions given in source

prolog(":- op(600,fx,p).\np q.").
prolog(":- module(a,[op(600,fx,p)]).\np q.").
prolog(":- module(a,[b,op(600,fx,p),c]).\np q.").
prolog(":- module(a,[b,op(600,fx,p),c/2]).\np q.").
