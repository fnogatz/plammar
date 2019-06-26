:- module('plammar/pt_ast', [
    pt_ast/5
  ]).

:- use_module(library(plammar/util)).
:- use_module(library(plammar/state)).
:- use_module(library(plammar/format_space)).
:- use_module(library(plammar/format_check)).

:- discontiguous
  pt_ast/5,
  pt_kind/6,
  pt/5.

% prolog
pt_ast(Opts, S0, SN, prolog([shebang(['#','!',comment_text(Atom,_),_])|PT_List]), prolog([shebang(Atom)|AST_List])) :-
  !,
  check(Opts, S0, S1, shebang),
  state_space(S1, S2, rows(1)),
  pt_kind(Opts, S2, SN, prolog, PT_List, AST_List).
pt_ast(Opts, S0, SN, prolog(PT_List), prolog(AST_List)) :-
  pt_kind(Opts, S0, SN, prolog, PT_List, AST_List).

pt_kind(_Opts, S0, S0, prolog, [], []).
pt_kind(_Opts, S0, S0, prolog, [layout_text_sequence(_)], []).
pt_kind(Opts, S0, SN, prolog, [PT|PTs], [AST|ASTs]) :-
  set_context(S0, S1, indent, level(0)),
  pt_ast(Opts, S1, S2, PT, AST),
  pt_kind(Opts, S2, SN, prolog, PTs, ASTs).

% clause_term
pt_ast(Opts, S0, SN,
  clause_term([Term, end(PT_End)]),
  fact(AST_Term)
) :-
  pt_ast(Opts, S0, S1, Term, AST_Term),
  pt(Opts, S1, S2, end, PT_End),
  set_context(S2, S3, after_clause),
  inc_state(S3, SN, statistics, facts).

% directive_term: rule
pt_ast(Opts, S0, SN,
  directive_term([
    term(xfx, [
      PT_Head,
      op(atom(name(PT_Op))),
      PT_Body
    ]),
    end(PT_End)
  ]),
  rule(AST_Head, AST_Body)
) :-
  pt_ast(Opts, S0, S1, PT_Head, AST_Head),
  pt(Opts, S1, S2, ':-', PT_Op),
  set_context(S2, S3, after_rule_op),
  inc_context(S3, S4, indent, level),
  pt_kind(Opts, S4, S5, rule_body, PT_Body, AST_Body),
  pt(Opts, S5, S6, end, PT_End),
  set_context(S6, S7, after_clause),
  check_entity(Opts, S7, S8, max_subgoals, AST_Body),
  check_entity(Opts, S8, S9, max_rule_lines, [S0, S6]),
  inc_state(S9, SN, statistics, rules).

% directive_term: directive
pt_ast(Opts, S0, SN,
  directive_term([
    term(fx, [
      op(atom(name(PT_Op))),
      PT_Body
    ]),
    end(PT_End)
  ]),
  directive(AST_Body)
) :-
  pt(Opts, S0, S1, ':-', PT_Op),
  set_context(S1, S2, after_directive_op),
  pt_kind(Opts, S2, S3, directive_body, PT_Body, AST_Body),
  pt(Opts, S3, S4, end, PT_End),
  set_context(S4, S5, after_clause),
  inc_state(S5, SN, statistics, directives).

% term: atom
pt_ast(Opts, S0, SN,
  term(atom(name(PT_Name))),
  atom(Atom)
) :-
  Atom \== '{}',
  layout(Opts, S0, S1, PT_Name, PT),
  pt_kind(Opts, S1, SN, name_token, PT, Atom).

pt_kind(Opts, S0, SN, name_token, PT, Atom) :-
  PT = name_token(_, quoted_token(PT_Quoted_Token)),
  !,
  state_space(S0, S1, cols(1)), % opening single quotes
  % remove quote chars to use the same predicate for
  %   back quoted, single quoted, and double quoted
  append([Quote_Char|Quoteds], [Quote_Char], PT_Quoted_Token),
  quoted_items(Opts, S1, S2, Quoteds, Atom),
  state_space(S2, SN, cols(1)). % closing single quotes

pt_kind(_Opts, S0, SN, name_token, PT, Atom) :-
  PT = name_token(Atom, _),
  atom_length(Atom, Length),
  state_space(S0, SN, cols(Length)).

% term: empty dict with atom as tag
pt_ast(Opts, S0, SN,
  term([
    name(PT_Name),
    open_curly([open_curly_token(open_curly_char('{'))]),
    close_curly(PT_Close_Curly)
  ]),
  dict(atom(Atom), [])
) :-
  pt_ast(Opts, S0, S1, term(atom(name(PT_Name))), atom(Atom)),
  check(Opts, S1, S2, dicts),
  state_space(S2, S3, cols(1)), % '{'
  layout(Opts, S3, S4, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S4, SN, cols(1)). % '}'.

% term: non-empty dict with atom as tag
pt_ast(Opts, S0, SN,
  term([
    name(PT_Name),
    open_curly([open_curly_token(open_curly_char('{'))]),
    key_value_list(_PT_Key_Value_List),  %% TODO
    close_curly(PT_Close_Curly)
  ]),
  dict(atom(Atom), [])
) :-
  pt_ast(Opts, S0, S1, term(atom(name(PT_Name))), atom(Atom)),
  check(Opts, S1, S2, dicts),
  state_space(S2, S3, cols(1)), % '{'
  layout(Opts, S3, S4, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S4, SN, cols(1)). % '}'.

% term: empty dict with variable as tag
pt_ast(Opts, S0, SN,
  term([
    variable(PT_Variable),
    open_curly([open_curly_token(open_curly_char('{'))]),
    close_curly(PT_Close_Curly)
  ]),
  dict(AST_Variable, [])
) :-
  pt_ast(Opts, S0, S1, term(variable(PT_Variable)), AST_Variable),
  check(Opts, S1, S2, dicts),
  state_space(S2, S3, cols(1)), % '{'
  layout(Opts, S3, S4, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S4, SN, cols(1)). % '}'.

% term: non-empty dict with variable as tag
pt_ast(Opts, S0, SN,
  term([
    variable(PT_Variable),
    open_curly([open_curly_token(open_curly_char('{'))]),
    key_value_list(_PT_Key_Value_List),  %% TODO
    close_curly(PT_Close_Curly)
  ]),
  dict(AST_Variable, [])
) :-
  pt_ast(Opts, S0, S1, term(variable(PT_Variable)), AST_Variable),
  check(Opts, S1, S2, dicts),
  state_space(S2, S3, cols(1)), % '{'
  layout(Opts, S3, S4, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S4, SN, cols(1)). % '}'.

% term: double quoted list
pt_ast(Opts, S0, SN,
  term(double_quoted_list(PT_Double_Quoted_List)),
  double_quoted(Atom)
) :-
  layout(Opts, S0, S1, PT_Double_Quoted_List, PT),
  state_space(S1, S2, cols(1)), % opening double quotes
  % remove quote chars to use the same predicate for
  %   back quoted, single quoted, and double quoted
  PT = double_quoted_list_token(_, PT_Double_Quoted_List_Token),
  append([Quote_Char|Quoteds], [Quote_Char], PT_Double_Quoted_List_Token),
  quoted_items(Opts, S2, S3, Quoteds, Atom),
  state_space(S3, SN, cols(1)). % closing double quotes

% term: back quoted list
pt_ast(Opts, S0, SN,
  term(back_quoted_string(PT_Back_Quoted_String)),
  back_quoted(Atom)
) :-
  layout(Opts, S0, S1, PT_Back_Quoted_String, PT),
  state_space(S1, S2, cols(1)), % opening back quotes
  % remove quote chars to use the same predicate for
  %   back quoted, single quoted, and double quoted
  PT = back_quoted_string_token(_, PT_Back_Quoted_String_Token),
  append([Quote_Char|Quoteds], [Quote_Char], PT_Back_Quoted_String_Token),
  quoted_items(Opts, S2, S3, Quoteds, Atom),
  state_space(S3, SN, cols(1)). % closing back quotes

% term: empty curly brackets (6.3.1.3)
pt_ast(Opts, S0, SN,
  term(atom([
    open_curly(PT_Open_Curly),
    close_curly(PT_Close_Curly)
  ])),
  atom('{}')
) :-
  layout(Opts, S0, S1, PT_Open_Curly, open_curly_token(open_curly_char('{'))),
  state_space(S1, S2, cols(1)), % '{'
  layout(Opts, S2, S3, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S3, SN, cols(1)). % '}'

% term: curly bracketed term (6.3.6)
pt_ast(Opts, S0, SN,
  term([
    open_curly(PT_Open_Curly),
    PT_Term,
    close_curly(PT_Close_Curly)
  ]),
  prefix('{}', fx, AST_Term)
) :-
  layout(Opts, S0, S1, PT_Open_Curly, open_curly_token(open_curly_char('{'))),
  state_space(S1, S2, cols(1)), % '{'
  pt_ast(Opts, S2, S3, PT_Term, AST_Term),
  layout(Opts, S3, S4, PT_Close_Curly, close_curly_token(close_curly_char('}'))),
  state_space(S4, SN, cols(1)). % '}'

% term: integer
pt_ast(Opts, S0, SN,
  term(integer(PT_Integer)),
  integer(Integer)
) :-
  layout(Opts, S0, S1, PT_Integer, PT),
  PT = integer_token(Atom, Inner_PT),
  check_entity(Opts, S1, S2, digit_groups, Inner_PT),
  check_entity(Opts, S2, S3, single_quote_char_in_character_code_constant, Inner_PT),
  ( Inner_PT = character_code_constant(['0', single_quote_char('\''), single_quoted_character(PT_Single_Quoted_Character)]) ->
    check_entity(Opts, S3, S4, symbolic_chars, PT_Single_Quoted_Character)
  ; otherwise ->
    S4 = S3
  ),
  integer_number(Atom, Inner_PT, Integer),
  atom_length(Atom, Length),
  state_space(S4, SN, cols(Length)).

% term: float
pt_ast(Opts, S0, SN,
  term(float_number(PT_Float)),
  float(Float)
) :-
  layout(Opts, S0, S1, PT_Float, PT),
  PT = float_number_token(Atom, PT_Float_Number_Token),
  check_entity(Opts, S1, S2, integer_exponential_notation, PT_Float_Number_Token),
  atom_number(Atom, Float),
  atom_length(Atom, Length),
  state_space(S2, SN, cols(Length)).

% term: anonymous variable
pt_ast(Opts, S0, SN,
  term(variable(PT_Variable)),
  anonymous
) :-
  ( PT_Variable = [PT] ; PT_Variable = [_LTS, PT] ),
  PT = variable_token('_', anonymous_variable(_)),
  !,
  layout(Opts, S0, S1, PT_Variable, PT),
  state_space(S1, SN, cols(1)).

% term: named variable
pt_ast(Opts, S0, SN,
  term(variable(PT_Variable)),
  variable(Variable)
) :-
  ( PT_Variable = [PT] ; PT_Variable = [_LTS, PT] ),
  PT = variable_token(Variable, named_variable(_)),
  !,
  layout(Opts, S0, S1, PT_Variable, PT),
  atom_length(Variable, Length),
  state_space(S1, SN, cols(Length)).

% term: compound
pt_ast(Opts, S0, SN,
  term([
    atom(PT_Atom),
    open_ct(open_token(open_char('('))),
    arg_list(PT_Arg_List),
    close(PT_Close)
  ]),
  compound(Atom, AST_Arg_list)
) :-
  AST_Arg_list \== [],
  pt_ast(Opts, S0, S1, term(atom(PT_Atom)), Atom),
  state_space(S1, S2, cols(1)), % open_ct
  inc_context(S2, S3, indent, level),
  pt_kind(Opts, S3, S4, arg_list, PT_Arg_List, AST_Arg_list),
  dec_context(S4, S5, indent, level),
  pt(Opts, S5, SN, close, PT_Close).

% term: compound with zero arguments
pt_ast(Opts, S0, SN,
  term([
    atom(PT_Atom),
    open_ct(open_token(open_char('('))),
    close(PT_Close)
  ]),
  compound(Atom, [])
) :-
  pt_ast(Opts, S0, S1, term(atom(PT_Atom)), Atom),
  state_space(S1, S2, cols(1)), % open_ct
  check(Opts, S2, S3, compounds_with_zero_arguments),
  pt(Opts, S3, SN, close, PT_Close).

% arg_list
pt_kind(Opts, S0, SN, arg_list,
  arg(PT_Arg),
  [AST_Arg]
) :-
  pt_kind(Opts, S0, SN, arg, PT_Arg, AST_Arg).

pt_kind(Opts, S0, SN, arg_list,
  [arg(PT_Arg), comma(PT_Comma), arg_list(PT_Arg_List)],
  [AST_Arg|AST_Arg_List]
) :-
  pt_kind(Opts, S0, S1, arg, PT_Arg, AST_Arg),
  pt(Opts, S1, S2, comma, PT_Comma),
  set_context(S2, S3, after_arglist_comma),
  pt_kind(Opts, S3, SN, arg_list, PT_Arg_List, AST_Arg_List).

% arg = atom
pt_kind(Opts, S0, SN, arg,
  atom(PT_Atom),
  AST_Atom
) :-
  pt_ast(Opts, S0, SN, term(atom(PT_Atom)), AST_Atom).

% arg = term
pt_kind(Opts, S0, SN, arg,
  PT_Term,
  AST_Term
) :-
  ( PT_Term = term(_) ; PT_Term = term(_,_) ),
  !,
  pt_ast(Opts, S0, SN, PT_Term, AST_Term).

% term: infix
pt_ast(Opts, S0, SN,
  term(Spec, [
    PT_Term1,
    op(PT_Op),
    PT_Term2
  ]),
  infix(Atom, Spec, AST_Term1, AST_Term2)
) :-
  spec_class(Spec, infix),
  pt_ast(Opts, S0, S1, PT_Term1, AST_Term1),
  pt_kind(Opts, S1, S2, op, PT_Op, Atom),
  pt_ast(Opts, S2, SN, PT_Term2, AST_Term2).

% term: prefix
pt_ast(Opts, S0, SN,
  term(Spec, [
    op(PT_Op),
    PT_Term
  ]),
  prefix(Atom, Spec, AST_Term)
) :-
  spec_class(Spec, prefix),
  pt_kind(Opts, S0, S1, op, PT_Op, Atom),
  pt_ast(Opts, S1, SN, PT_Term, AST_Term).

% term: postfix
pt_ast(Opts, S0, SN,
  term(Spec, [
    PT_Term,
    op(PT_Op)
  ]),
  postfix(Atom, Spec, AST_Term)
) :-
  spec_class(Spec, postfix),
  pt_ast(Opts, S0, S1, PT_Term, AST_Term),
  pt_kind(Opts, S1, SN, op, PT_Op, Atom).

% op
pt_kind(Opts, S0, SN, op,
  atom(name(PT_Name)),
  Atom
) :-
  Atom \== ',',
  layout(Opts, S0, S1, PT_Name, PT),
  PT = name_token(Atom, _),
  atom_length(Atom, Length),
  state_space(S1, SN, cols(Length)).

pt_kind(Opts, S0, SN, op,
  comma(PT_Comma),
  ','
) :-
  layout(Opts, S0, S1, PT_Comma, PT),
  PT = comma_token(_),
  state_space(S1, SN, cols(1)).

pt_kind(Opts, S0, SN, op,
  ht_sep(PT_Ht_Sep),
  '|'
) :-
  layout(Opts, S0, S1, PT_Ht_Sep, PT),
  PT = head_tail_separator_token(head_tail_separator_char('|')),
  state_space(S1, SN, cols(1)).

% term in brackets (using open)
pt_ast(Opts, S0, SN,
  term([
    open(PT_Open),
    PT_Term,
    close(PT_Close)
  ]),
  AST_Term
) :-
  pt(Opts, S0, S1, open, PT_Open),
  pt_ast(Opts, S1, S2, PT_Term, AST_Term),
  pt(Opts, S2, SN, close, PT_Close).

% term in brackets (using open_ct)
pt_ast(Opts, S0, SN,
  term([
    open_ct(PT_Open),
    PT_Term,
    close(PT_Close)
  ]),
  AST_Term
) :-
  pt(Opts, S0, S1, open_ct, PT_Open),
  pt_ast(Opts, S1, S2, PT_Term, AST_Term),
  pt(Opts, S2, SN, close, PT_Close).

% term: empty list []
pt_ast(Opts, S0, SN,
  term(atom([
    open_list(PT_Open_List),
    close_list(PT_Close_List) 
  ])),
  list([], eol)
) :-
  pt(Opts, S0, S1, open_list, PT_Open_List),
  pt(Opts, S1, SN, close_list, PT_Close_List).

% term: list
pt_ast(Opts, S0, SN,
  term([
    open_list(PT_Open_List),
    items(PT_Items),
    close_list(PT_Close_List)
  ]),
  list(AST_Items, EOL)
) :-
  AST_Items = [_|_], % at least one element
  pt(Opts, S0, S1, open_list, PT_Open_List),
  pt_kind(Opts, S1, S2, items, PT_Items, list(AST_Items, EOL)),
  pt(Opts, S2, SN, close_list, PT_Close_List).

% items
pt_kind(Opts, S0, SN, items, arg(PT_Arg), list([AST_Arg],eol)) :-
  pt_ast(Opts, S0, SN, PT_Arg, AST_Arg).

pt_kind(Opts, S0, SN, items,
  [arg(PT_Arg), comma(PT_Comma), items(PT_Item_List)],
  list([AST_Arg|AST_Item_List],EOL)
) :-
  AST_Item_List = [_|_], % rest not empty
  pt_ast(Opts, S0, S1, PT_Arg, AST_Arg),
  pt(Opts, S1, S2, comma, PT_Comma),
  pt_kind(Opts, S2, SN, items, PT_Item_List, list(AST_Item_List, EOL)).

pt_kind(Opts, S0, SN, items,
  [arg(PT_Arg1), ht_sep(PT_Ht_Sep), arg(PT_Arg2)],
  list([AST_Arg1],AST_Arg2)
) :-
  AST_Arg2 \== eol,
  pt_ast(Opts, S0, S1, PT_Arg1, AST_Arg1),
  pt(Opts, S1, S2, ht_sep, PT_Ht_Sep),
  pt_ast(Opts, S2, SN, PT_Arg2, AST_Arg2).

% rule body
pt_kind(Opts, S0, SN, rule_body,
  term(xfy, [
    PT_Term1,
    op(PT_Op),
    PT_Term2
  ]),
  AST
) :-
  pt_ast(Opts, S0, S1, PT_Term1, AST_Term1),
  pt_kind(Opts, S1, S2, op, PT_Op, Atom),
  ( Atom = ',' ->
    set_context(S2, S3, after_subgoal),
    AST = [AST_Term1|Rest],
    pt_kind(Opts, S3, SN, rule_body, PT_Term2, Rest)
  ; otherwise ->
    pt_ast(Opts, S2, SN, PT_Term2, AST_Term2),
    AST = [infix(Atom, xfy, AST_Term1, AST_Term2)]
  ).
pt_kind(Opts, S0, SN, rule_body, PT, [AST]) :-
  PT \= term(xfy, _),
  pt_ast(Opts, S0, SN, PT, AST).

% directive body
pt_kind(Opts, S0, SN, directive_body,
  term(xfy, [
    PT_Term1,
    op(PT_Op),
    PT_Term2
  ]),
  AST
) :-
  pt_ast(Opts, S0, S1, PT_Term1, AST_Term1),
  pt_kind(Opts, S1, S2, op, PT_Op, Atom),
  ( Atom = ',' ->
    set_context(S2, S3, after_subgoal),
    AST = [AST_Term1|Rest],
    pt_kind(Opts, S3, SN, directive_body, PT_Term2, Rest)
  ; otherwise ->
    pt_ast(Opts, S2, SN, PT_Term2, AST_Term2),
    AST = [infix(Atom, xfy, AST_Term1, AST_Term2)]
  ).
pt_kind(Opts, S0, SN, directive_body, PT, [AST]) :-
  PT \= term(xfy, _),
  pt_ast(Opts, S0, SN, PT, AST).

% end
pt(Opts, S0, SN, end, PT) :-
  PT_End_Token = end_token(end_char('.')),
  layout(Opts, S0, S1, PT, PT_End_Token),
  state_space(S1, SN, cols(1)).

% open
pt(Opts, S0, SN, open, PT) :-
  PT_Open_Token = open_token(open_char('(')),
  layout(Opts, S0, S1, PT, PT_Open_Token),
  state_space(S1, SN, cols(1)).

% open_ct
pt(_Opts, S0, SN, open_ct, PT) :-
  PT = open_token(open_char('(')),
  state_space(S0, SN, cols(1)).

% close
pt(Opts, S0, SN, close, PT) :-
  PT_Close_Token = close_token(close_char(')')),
  layout(Opts, S0, S1, PT, PT_Close_Token),
  state_space(S1, SN, cols(1)).

% comma
pt(Opts, S0, SN, comma, PT) :-
  PT_Comma_Token = comma_token(comma_char(',')),
  layout(Opts, S0, S1, PT, PT_Comma_Token),
  state_space(S1, SN, cols(1)).

% open_list
pt(Opts, S0, SN, open_list, PT) :-
  PT_Open_List_Token = open_list_token(open_list_char('[')),
  layout(Opts, S0, S1, PT, PT_Open_List_Token),
  state_space(S1, SN, cols(1)).

% close_list
pt(Opts, S0, SN, close_list, PT) :-
  PT_Close_List_Token = close_list_token(close_list_char(']')),
  layout(Opts, S0, S1, PT, PT_Close_List_Token),
  state_space(S1, SN, cols(1)).

% ht_sep
pt(Opts, S0, SN, ht_sep, PT) :-
  PT_Head_Tail_Separator_Token = head_tail_separator_token(head_tail_separator_char('|')),
  layout(Opts, S0, S1, PT, PT_Head_Tail_Separator_Token),
  state_space(S1, SN, cols(1)).

% ":-" in directive_term
pt(Opts, S0, SN, ':-', PT) :-
  PT_Directive_Sep = name_token(':-', graphic_token([
    graphic_token_char(graphic_char(':')),
    graphic_token_char(graphic_char('-'))
  ])),
  layout(Opts, S0, S1, PT, PT_Directive_Sep),
  state_space(S1, SN, cols(2)).


% Default transformations
pt_ast(_, S0, S0, Q, R) :-
  var(R),
  option(pos(L0:C0), S0, na:na),
  Q =.. [Kind|_],
  setof(
    Type,
    [Opts,S0,S1,A,B,Argument,Body]^(
      clause(pt_ast(Opts,S0,S1,A,B), Body),
      \+ var(A),
      A =.. [Type, Argument]
    ),
    Types
  ),
  Msg = 'No pt_ast/5 rule defined for ~w. Use one of ~w. Complete call was at line ~d, column ~d:~n~w.',
  warning(Msg, [Kind, Types, L0, C0, Q]),
  R = Q.

pt_kind(_, S0, S0, Target, Q, R) :-
  var(R),
  option(pos(L0:C0), S0, na:na),
  Msg = 'No pt_kind/6 rule defined for ~w. Complete call was at line ~d, column ~d:~n~w.',
  warning(Msg, [Target, L0, C0, Q]),
  R = Q.

pt(_, S0, S0, Target, Q) :-
  option(pos(L0:C0), S0, na:na),
  Msg = 'No pt/5 rule defined for ~w. Complete call was at line ~d, column ~d:~n~w.',
  warning(Msg, [Target, L0, C0, Q]).
