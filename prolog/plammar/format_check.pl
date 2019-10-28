:- module('plammar/format_check', [
    check/4,
    check_entity/5
  ]).

:- use_module(state).
:- use_module(options).
:- use_module(util).

check(Opts, S0, SN, Prop) :-
  What =.. [Prop, Value/Secondary],
  style_option(What, Opts),
  check(Opts, S0, SN, Prop, Value, Secondary).

check(_Opts, S0, S0, _Prop, ignore, _Secondary) :-
  % just do nothing
  true.


%% check: max_line_length

check(_Opts, S0, S0, max_line_length, Integer, _Secondary) :-
  integer(Integer),
  option(pos(_:C0), S0),
  ( C0 > Integer ->
    writeln('too wide...') %% TODO
  ; otherwise ->
    true
  ).

check(Opts, S0, SN, max_line_length, call(Goal), _Secondary) :-
  option(pos(_:C0), S0),
  Goal =.. [Pred|Args],
  append([Opts, S0, SN|Args], [C0], All_Args),
  Full_Goal =.. [Pred|All_Args],
  call(Full_Goal).


%% check: indent

check(_Opts, S0, SN, indent, Integer, _Secondary) :-
  integer(Integer),
  get_context(S0, indent, level(Indent_Level), 0),
  get_context(S0, layout, leading_spaces(Leading_Spaces), 0),
  get_context(S0, layout, leading_tabs(Leading_Tabs), 0),
  Expected is Indent_Level * Integer,
  ( % used tabs instead of spaces?
    Leading_Tabs > 0 ->
    state_warn(S0, S1, [prop(indent_by_spaces)])
  ; Leading_Spaces \= Expected ->
    state_warn(S0, S1, [prop(indent), expected(Expected), found(Leading_Spaces)])
  ; otherwise ->
    S1 = S0
  ),
  SN = S1.

check(_Opts, S0, SN, indent, '\t', _Secondary) :-
  get_context(S0, indent, level(Indent_Level), 0),
  get_context(S0, layout, leading_spaces(Leading_Spaces), 0),
  get_context(S0, layout, leading_tabs(Leading_Tabs), 0),
  Expected is Indent_Level,
  ( % used spaces instead of tabs?
    Leading_Spaces > 0 ->
    state_warn(S0, S1, [prop(indent_by_tabs)])
  ; Leading_Tabs \= Expected ->
    state_warn(S0, S1, [prop(indent), expected(Expected), found(Leading_Tabs)])
  ; otherwise ->
    S1 = S0
  ),
  SN = S1.

check(Opts, S0, SN, indent, call(Goal), _Secondary) :-
  get_context(S0, indent, level(Indent_Level), 0),
  get_context(S0, layout, leading_spaces(Leading_Spaces), 0),
  get_context(S0, layout, leading_tabs(Leading_Tabs), 0),
  Value = [
    level(Indent_Level),
    found_spaces(Leading_Spaces),
    found_tabs(Leading_Tabs)
  ],
  Goal =.. [Pred|Args],
  append([Opts, S0, SN|Args], [Value], All_Args),
  Full_Goal =.. [Pred|All_Args],
  call(Full_Goal).


%% check: dicts

check(_Opts, S0, SN, dicts, Setting, _Secondary) :-
  no(Setting),
  state_warn(S0, SN, [prop(dict), msg('Avoid using dicts')]).
check(_Opts, S0, S0, dicts, _Setting, _Secondary).


%% check: compounds_with_zero_arguments

check(_Opts, S0, SN, compounds_with_zero_arguments, Setting, _Secondary) :-
  no(Setting),
  state_warn(S0, SN, [prop(compounds_with_zero_arguments)]).
check(_Opts, S0, S0, compounds_with_zero_arguments, _Setting, _Secondary).


%% check: shebang
check(_Opts, S0, SN, shebang, Setting, _Secondary) :-
  no(Setting),
  state_warn(S0, SN, [prop(shebang)]).
check(_Opts, S0, S0, shebang, _Setting, _Secondary).



%% check: default
check(_Opts, S0, S0, Prop, Value, _) :-
  warning('Unknown value "~w" for checked property "~w".', [Value, Prop]),
  !.


check_entity(Opts, S0, SN, Prop, Entity) :-
  What =.. [Prop, Value/Secondary],
  style_option(What, Opts),
  check_entity(Opts, S0, SN, Prop, Value, Secondary, Entity).

check_entity(_Opts, S0, S0, _Prop, ignore, _Secondary, _Entity) :-
  % just do nothing
  true.


%% entity: max_subgoals

check_entity(_Opts, S0, S0, max_subgoals, inf, _Secondary, _Entity).

check_entity(Opts, S0, SN, max_subgoals, call(Goal), _Secondary, Subgoals) :-
  length(Subgoals, Length),
  Goal =.. [Pred|Args],
  append([Opts, S0, SN|Args], [Length], All_Args),
  Full_Goal =.. [Pred|All_Args],
  once(call(Full_Goal)).


%% entity: max_rule_lines

check_entity(_Opts, S0, S0, max_rule_lines, inf, _Secondary, _Entity).

check_entity(Opts, S0, SN, max_rule_lines, call(Goal), _Secondary, [S_Begin, S_End]) :-
  option(pos(L0:_), S_Begin),
  option(pos(L1:_), S_End),
  Length is L1-L0,
  Goal =.. [Pred|Args],
  append([Opts, S0, SN|Args], [Length], All_Args),
  Full_Goal =.. [Pred|All_Args],
  once(call(Full_Goal)).


%% entity: integer_exponential_notation

check_entity(Opts, S0, SN, integer_exponential_notation, Setting, _Secondary, PT_Float_Number_Token) :-
  PT_Float_Number_Token = [integer_constant(PT_Integer_Constant), exponent(PT_Exponent)],
  no(Setting),
  plammar:float_number_token(Opts, float_number_token(Found, PT_Float_Number_Token), _, []),
  plammar:float_number_token(Opts, float_number_token(Expected, [integer_constant(PT_Integer_Constant), fraction([decimal_point_char('.'), decimal_digit_char('0')]), exponent(PT_Exponent)]), _, []),
  use_msg(Found, Expected, Msg, 'for ISO conformity'),
  state_warn(S0, SN, [prop(integer_exponential_notation), msg(Msg)]).
check_entity(_Opts, S0, S0, integer_exponential_notation, _Setting, _Secondary, _PT).


%% entity: digit_groups

check_entity(Opts, S0, SN, digit_groups, Setting, _Secondary, PT) :-
  no(Setting),
  PT =.. [Type, List0],
  delete(List0, underscore_char(_), List1),
  delete(List1, space_char(_), List2),
  List0 \== List2,
  PT_Expected =.. [Type, List2],
  plammar:integer_token(Opts, integer_token(Found, PT), _, []),
  plammar:integer_token(Opts, integer_token(Expected, PT_Expected), _, []),
  use_msg(Found, Expected, Msg, 'for ISO conformity'),
  state_warn(S0, SN, [prop(digit_groups), msg(Msg)]).
check_entity(_Opts, S0, S0, digit_groups, _Setting, _Secondary, _PT).


%% entity: single_quote_char_in_character_code_constant
check_entity(_Opts, S0, SN, single_quote_char_in_character_code_constant, Setting, _Secondary, PT) :-
  no(Setting),
  PT = character_code_constant(['0', single_quote_char('\''), single_quoted_character(single_quote_char('\''))]),
  % PT_Expected = character_code_constant([0,single_quote_char('),single_quoted_character(non_quote_char(meta_escape_sequence([backslash_char(\),meta_char(single_quote_char('))])))])
  Found = '0\'\'',
  Expected = '0\'\\\'',
  use_msg(Found, Expected, Msg, 'for ISO conformity'),
  state_warn(S0, SN, [prop(single_quote_char_in_character_code_constant), msg(Msg)]).
check_entity(_Opts, S0, S0, single_quote_char_in_character_code_constant, _Setting, _Secondary, _PT).


%% entity: symbolic_chars
check_entity(_Opts, S0, SN, symbolic_chars, Setting, Secondary, PT) :-
  no(Setting),
  PT = non_quote_char(control_escape_sequence([backslash_char('\\'),symbolic_control_char(PT_Symbolic_Control_Char)])),
  option(disallow(Disallow), Secondary, ['c','e','s']),
  PT_Symbolic_Control_Char =.. [_Type, Char],
  member(Char, Disallow),
  state_warn(S0, SN, [prop(symbolic_chars), char(Char)]).
check_entity(_Opts, S0, S0, symbolic_chars, _Setting, _Secondary, _PT).


%% entity: unicode_character_escape
check_entity(_Opts, S0, SN, unicode_character_escape, Setting, _Secondary, PT) :-
  no(Setting),
  PT = non_quote_char(unicode_escape_sequence(_)),
  state_warn(S0, SN, [prop(unicode_character_escape)]).
check_entity(_Opts, S0, S0, unicode_character_escape, _Setting, _Secondary, _PT).


%% entity: missing_closing_backslash_in_character_escape
check_entity(_Opts, S0, SN, missing_closing_backslash_in_character_escape, Setting, _Secondary, PT) :-
  no(Setting),
  PT = non_quote_char(hexadecimal_escape_sequence([backslash_char('\\'), symbolic_hexadecimal_char('x')|Digits])),
  \+ append(_, [backslash_char('\\')], Digits),
  state_warn(S0, SN, [prop(missing_closing_backslash_in_character_escape)]).
check_entity(_Opts, S0, SN, missing_closing_backslash_in_character_escape, Setting, _Secondary, PT) :-
  no(Setting),
  PT = non_quote_char(octal_escape_sequence([backslash_char('\\')|Digits])),
  \+ append(_, [backslash_char('\\')], Digits),
  state_warn(S0, SN, [prop(missing_closing_backslash_in_character_escape)]).
check_entity(_Opts, S0, S0, missing_closing_backslash_in_character_escape, _Setting, _Secondary, _PT).


%% entity: default
check_entity(_Opts, S0, S0, Prop, Value, _, _Entity) :-
  warning('Unknown value "~w" for checked property "~w".', [Value, Prop]),
  !.
