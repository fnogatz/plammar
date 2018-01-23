:- module(ast, [
    tree/3,
    tree/4
  ]).

tree(Body, In, Tree) :-
  tree(Body, In, Tree, []).

tree(Body, In, Tree, Rest) :-
  phrase(Body, In, [Tree|Rest]).

tree_from_file(Body, Filename, Tree) :-
  read_file_to_codes(Filename, Codes, []),
  maplist(char_code, Chars, Codes),
  tree(Body, Chars, Tree).

nil_symbol([]). % might change in the future

is_disj((A ; B), A, B).
is_disj((A | B), A, B).
is_disj(Disj) :- is_disj(Disj, _, _).

is_conj((A , B), A, B).
is_conj(Conj) :- is_conj(Conj, _, _).

is_sequence(*(_)).

is_terminal([ _ ]).

node(H0, Results, Res) :-
  atom_chars(H0, H0_Chars),
  ( append(Front, ['_'], H0_Chars) ->
    Without_Underscore_Suffix = Front
  ;
    Without_Underscore_Suffix = H0_Chars
  ),
  atom_chars(Atom, Without_Underscore_Suffix),
  Res =.. [Atom, Results].

expand_dcg_disj((H0 --> B0), Rules) :-
  \+is_disj(B0),
  expand_dcg((H0 --> B0), Expanded_0),
  Rules = [Expanded_0].

expand_dcg_disj((H0 --> Disj), Rules) :-
  is_disj(Disj, B0_0, B0_1),
  expand_dcg_disj((H0 --> B0_1), Rules_1),
  expand_dcg((H0 --> B0_0), Expanded_0),
  Rules = [Expanded_0 | Rules_1].

dcg_body_result(B0, Body, List_of_Res) :-
  is_sequence(B0),
  Read_Res = [ List_of_Res ],
  Body = (B0, Read_Res).

dcg_body_result(B0, Body, [Single_Res]) :-
  \+is_sequence(B0),
  Read_Res = [ Single_Res ],
  Body = (B0, Read_Res).

expand_dcg_conj_body(B0, Body, Reses) :-
  \+is_conj(B0),
  dcg_body_result(B0, Body, Reses).

expand_dcg_conj_body(Conj, (Body_0, Body_Rest), Results) :-
  is_conj(Conj, B0_0, B0_1),
  dcg_body_result(B0_0, Body_0, Reses),
  expand_dcg_conj_body(B0_1, Body_Rest, Rec_Results),
  append(Reses, Rec_Results, Results).

expand_dcg((H0 --> B0), Expanded) :-
  expand_term((H0 --> B0), Expanded).

term_expansion((H0 --> Disj), Rules) :-
  is_disj(Disj),
  expand_dcg_disj((H0 --> Disj), Rules).

term_expansion((H0 --> Conj), Expanded) :-
  is_conj(Conj),
  expand_dcg_conj_body(Conj, Body, Results),
  node(H0, Results, Res),
  dcg_translate_rule((H0, [Res] --> Body), Expanded).

term_expansion((H0 --> [T]), Expanded) :-
  node(H0, T, Res),
  dcg_translate_rule((H0, [Res] --> [T]), Expanded).

term_expansion((H0 --> B0), Expanded) :-
  \+ is_conj(B0),
  \+ is_disj(B0),
  \+ is_terminal(B0),
  % single body element
  node(H0, Inner_Res, Res),
  dcg_translate_rule((H0, [Res] --> B0, [Inner_Res]), Expanded).


%% op `*` to denote any number of occurences
:- op(800, fy, *).
*(_, In, [[]|In]).
*(Body, In, [Res|Rest]) :-
  phrase(Body, In, [Single_Res|C]),
  *(Body, C, D),
  D = [Rec_Res|Rest],
  Res = [Single_Res|Rec_Res].


%% op `?` to denote optional occurence
:- op(800, fy, ?).
?(A, In, [R|In]) :-
  nil_symbol(Nil),
  R =.. [A, Nil].
?(A, In, [Single_Res|C]) :-
  phrase(A, In, [Single_Res|C]).


%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(In, [token(Token_Res)|Rest]) :-
  token_(In, [token(Token_Res)|Rest]),
  (
    % empty rest list
    Rest = []
  ;
    Rest = [_One_More_Element|Rests],
    % consuming one element does not succeed
    \+ token_(In, [token(_)|Rests])
  ).


:- consult('iso-grammar.pl').

test --> *alphanumeric_char.
