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
    p_text([clause_term([term(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),end([end_token(end_char('.'))])])])
  ].

'"a. b. c." has a single parse tree' :-
  findall(
    PT,
    prolog_parsetree(string("a. b. c."), PT),
    PTs
  ),
  PTs = [
    p_text([
      clause_term([term(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),
        end([end_token(end_char(.))])]),
      clause_term([term(atom(name([layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token(letter_digit_token([small_letter_char(b)]))]))),
        end([end_token(end_char(.))])]),
      clause_term([term(atom(name([layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
          name_token(letter_digit_token([small_letter_char(c)]))]))),
        end([end_token(end_char(.))])])])
  ].

'"a :- b." has a single parse tree' :-
  findall(
    PT,
    prolog_parsetree(string("a :- b."), PT),
    PTs
  ),
  PTs = [
    p_text([directive_term([term(xfx,[term(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char(' ')))]),name_token(graphic_token([graphic_token_char(graphic_char(':')),graphic_token_char(graphic_char('-'))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char(' ')))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char('.'))])])])
  ].

'"a :- b." cannot be parsed when ISO operators are not used'(fail) :-
  prolog_parsetree(string("a :- b."), _PT, [ iso_operators(no) ]).

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


'"f :- a." is valid for f being an operator fy@1100' :-
  Options = [ operators([ op(1100,fy,f) ]) ],
  prolog_parsetrees(string("f :- a."), PTs, Options),
  length(PTs, 1).

'"f :- a." is valid for f being an operator fx@1100' :-
  Options = [ operators([ op(1100,fx,f) ]) ],
  prolog_parsetrees(string("f :- a."), PTs, Options),
  length(PTs, 1).

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
  PT = p_text([clause_term([term(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),end([end_token(end_char('.'))])])]),
  findall(
    String,
    prolog_parsetree(string(String), PT),
    Strings
  ),
  Strings = ["a."].

%% Part III: automatic deterministic conversion string->parsetree->string

prolog("a.").
prolog("a. b.").

prolog("a(b).").
prolog("a(1).").
prolog("a(1.2).").
prolog("a(X).").
prolog("a(_).").
prolog("a(1,2).").
prolog("a( 1 , 2 ).").

prolog("a :- b.").

prolog("list([]).").
prolog("list([X]).").
prolog("list([a,1]).").

prolog("member(X,[X|_]).").
prolog("member(X,[_|Xs]) :- member(X,Xs).").
