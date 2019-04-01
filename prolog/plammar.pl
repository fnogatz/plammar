:- module(plammar, [
    tree/3,
    tree/4,
    prolog_tokens/2,
    prolog_tokens/3,
    prolog_parsetree/2,
    prolog_parsetree/3
  ]).

:- use_module(library(plammar/environments)).
:- use_module(library(plammar/util)).
:- use_module(library(plammar/options)).

:- use_module(library(dcg4pt)).
:- use_module(library(clpfd)).

prolog_tokens(A, B) :-
  prolog_tokens(A, B, []).

prolog_tokens(string(String), Tokens, Options) :-
  !,
  I0 = string_chars(String, Chars),
  I1 = prolog_tokens(chars(Chars), Tokens, Options),
  ( \+ var(String) -> Instructions = (I0, I1)
  ; Instructions = (I1, I0) ),
  call(Instructions).

prolog_tokens(file(File), Tokens, Options) :-
  \+ var(File),
  !,
  open(File, read, Stream),
  prolog_tokens(stream(Stream), Tokens, Options),
  close(Stream).

prolog_tokens(stream(Stream), Tokens, Options) :-
  \+ var(Stream),
  !,
  read_string(Stream, _Length, String),
  prolog_tokens(string(String), Tokens, Options).

prolog_tokens(chars(Chars), Tokens, User_Options) :-
  !,
  normalise_options(prolog_tokens, User_Options, Options),
  prolog_tokens_(chars(Chars), Tokens, Options),
  revise_options(prolog_tokens, Options).

prolog_tokens(_, _, _) :-
  !,
  setof(
    Type,
    [Selector,Argument,Body,A,B]^(
      clause(prolog_tokens(Selector,A,B), Body),
      \+ var(Selector),
      Selector =.. [Type, Argument]
    ),
    Types 
  ),
  warning('Use one of input formats string ~w', Types).

prolog_tokens_(chars(Chars), Tokens, Options) :-
  phrase(plammar:term(Options, term(Tokens)), Chars, []).

prolog_parsetree(A, B) :-
  prolog_parsetree(A, B, []).

prolog_parsetree(string(String), PT, Options) :-
  \+ var(String),
  !,
  string_chars(String, Chars),
  prolog_parsetree(chars(Chars), PT, Options).
prolog_parsetree(string(String), PT, Options) :-
  \+ var(PT),
  !,
  prolog_parsetree(chars(Chars), PT, Options),
  string_chars(String, Chars).

prolog_parsetree(file(File), PT, Options) :-
  \+ var(File),
  !,
  open(File, read, Stream),
  prolog_parsetree(stream(Stream), PT, Options),
  close(Stream).

prolog_parsetree(stream(Stream), PT, Options) :-
  \+ var(Stream),
  !,
  read_string(Stream, _Length, String),
  prolog_parsetree(string(String), PT, Options).

prolog_parsetree(chars(Chars), PT, User_Options) :-
  !,
  normalise_options(prolog_parsetree, User_Options, Options),
  prolog_parsetree_(chars(Chars), PT, Options),
  revise_options(prolog_parsetree, Options).

prolog_parsetree(tokens(Tokens), PT, User_Options) :-
  !,
  normalise_options(prolog_parsetree, User_Options, Options),
  prolog(Options, PT, Tokens),
  revise_options(prolog_parsetree, Options).


prolog_parsetree(_, _, _) :-
  !,
  setof(
    Type,
    [Selector,Argument,Body,A,B]^(
      clause(prolog_parsetree(Selector,A,B), Body),
      \+ var(Selector),
      Selector =.. [Type, Argument]
    ),
    Types 
  ),
  warning('Use one of input formats ~w', [Types]).

prolog_parsetree_(chars(Chars), PT, Options) :-
  I0 = prolog_tokens(chars(Chars), Tokens, Options),
  I1 = prolog(Options, PT, Tokens),
  ( \+ var(Chars) -> Instructions = (I0, !, I1)
  ; Instructions = (I1, !, I0) ),
  call(Instructions).


pp(A) :-
  print_term(A, [indent_arguments(2),tab_width(0)]).

tree(Body, In, Tree) :-
  tree(Body, In, Tree, []).

tree(Body, In, Tree, Rest) :-
  Body =.. BodyList,
  append(BodyList, [Tree], BodyWithResList),
  BodyWithRes =.. BodyWithResList,
  phrase(BodyWithRes, In, Rest).

tree_from_file(Body, Filename, Tree) :-
  read_file_to_codes(Filename, Codes, []),
  maplist(char_code, Chars, Codes),
  tree(Body, Chars, Tree).


%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(Opts, Tree, In, Rest) :-
  \+ var(In), !,
  token_(Opts, token_(Tree), In, Rest),
  Some_More_Elements = [_|_], % at least one element
  \+((
    token_(Opts, _, In, Shorter_Rest),
    append(Some_More_Elements, Shorter_Rest, Rest)
  )).
token(Opts, Tree, In, Rest) :-
  \+ var(Tree), !,
  token_(Opts, token_(Tree), In, Rest).
token(_Opts, Tree, In, Rest) :-
  var(Tree), var(In), !,
  warning('Parse tree AND input unbound; this might not work as expected!'),
  token_(token_(Tree), In, Rest).

:- op(600, xfx, token).
:- discontiguous plammar:token/4.

user:term_expansion(X1 token Opts --> Y1, [Rule]) :-
  atom_concat(X1, '_token', X1_token),
  X1_token_with_Opts =.. [X1_token, Opts],
  dcg4pt:dcg4pt_rule_to_dcg_rule(X1_token_with_Opts --> Y1, X2 --> Y2),
  dcg_translate_rule(X2 --> Y2, Expanded_DCG_Rule),
  Expanded_DCG_Rule = (
    Expanded_DCG_Rule_Head :-
      Expanded_DCG_Rule_Body
  ),
  Expanded_DCG_Rule_Head =.. [X1_token, Opts, Initial_Tree, In, Out],
  Initial_Tree =.. [X1_token, Inner_Tree],
  New_DCG_Rule_Head =.. [X1_token, Opts, New_Tree, In, Out],
  New_Tree =.. [X1_token, Consumed, Inner_Tree],
  Rule = (
    New_DCG_Rule_Head :-
      Expanded_DCG_Rule_Body,
      ( var(Consumed) ->
        append(Consumed_Chars, Out, In),
        atom_chars(Consumed, Consumed_Chars)
      ; true )
  ).

:- op(600, xf, wrap_text).

user:term_expansion(Head wrap_text --> Y1, [Rule]) :-
  dcg4pt:dcg4pt_rule_to_dcg_rule(Head --> Y1, X2 --> Y2),
  dcg_translate_rule(X2 --> Y2, Expanded_DCG_Rule),
  Expanded_DCG_Rule = (
    Expanded_DCG_Rule_Head :-
      Expanded_DCG_Rule_Body
  ),
  Expanded_DCG_Rule_Head =.. [X1_token, Opts, Initial_Tree, In, Out],
  Initial_Tree =.. [X1_token, Inner_Tree],
  New_DCG_Rule_Head =.. [X1_token, Opts, New_Tree, In, Out],
  New_Tree =.. [X1_token, Consumed, Inner_Tree],
  Rule = (
    New_DCG_Rule_Head :-
      Expanded_DCG_Rule_Body,
      ( var(Consumed) ->
        append(Consumed_Chars, Out, In),
        atom_chars(Consumed, Consumed_Chars)
      ; true )
  ).

user:term_expansion(X1 --> Y1, [Rule]) :-
  dcg4pt:dcg4pt_rule_to_dcg_rule(X1 --> Y1, X2 --> Y2),
  dcg_translate_rule(X2 --> Y2, Rule).

/*
  *(DCGBody, Tree, In, Out) <-

  op `*` to denote any number of occurences.
  The distinction depending on the groundness
  of `In` is done only for performing reasons;
  if the input list `In` is given, it is more
  likely that many items can be consumed;
  whereas with an unbound `In` and given `Tree`
  we want to created the smallest possibilities
  at first.
*/
:- op(800, fy, *).
*(DCGBody, Tree, In, Out) :-
  % only if input list is given 
  \+var(In), !,
  % use `**` to consume as most as possible at first
  sequence('**', DCGBody, Tree, In, Out).
*(DCGBody, Tree, In, Out) :-
  % only if input list should be calculated
  var(In), !,
  % use `*` to produce as small as possible at first
  sequence('*', DCGBody, Tree, In, Out).

%% op `?` to denote zero or one occurences
:- op(800, fy, ?).
?(DCGBody, Tree, In, Out) :-
  sequence('?', DCGBody, Tree, In, Out).

:- load_files('plammar/dcg_token.pl', [module(plammar)]).
:- load_files('parser.pl', [module(plammar)]).
