:- module('plammar/format_space', [
    layout/5,
    quoted_items/5
  ]).

:- use_module(library(plammar/state)).
:- use_module(library(plammar/util)).
:- use_module(library(plammar/options)).
:- use_module(library(plammar/format_check)).

layout(Opts, S0, SN, [PT], PT) :-
  leading_layout(Opts, S0, SN, []).

layout(Opts, S0, SN, [layout_text_sequence(PT_Layout_Text_Sequence),PT], PT) :-
  leading_layout(Opts, S0, SN, PT_Layout_Text_Sequence).

leading_layout(Opts, S0, SN, PT_Layout_Text_Sequence) :-
  set_context(S0, S1, layout, [prev(null), newlines(0), leading_spaces(0), leading_tabs(0)]),
  layout_spaces(Opts, S1, S2, PT_Layout_Text_Sequence),
  del_context(S2, S2a, Context),
  del_context(S2a, S4, layout, newlines(Newlines), 0),
  get_context(S2, layout, leading_spaces(Spaces), 0),
  ( Context = after_clause,
    style_option(newline_after_clause(Newline_After_Clause/_), Opts),
    yes(Newline_After_Clause),
    Newlines = 0 ->
    state_warn(S4, S5, [prop(newline_after_clause)]),
    S6 = S5
  ; Context = after_rule_op,
    style_option(newline_after_rule_op(Newline_After_Rule_Op/_), Opts),
    yes(Newline_After_Rule_Op),
    Newlines = 0 ->
    state_warn(S4, S5, [prop(newline_after_rule_op)]),
    inc_context(S5, S6, layout, indent_level)
  ; Context = after_subgoal,
    style_option(newline_after_subgoal(Newline_After_Subgoal/_), Opts),
    yes(Newline_After_Subgoal),
    Newlines = 0 ->
    state_warn(S4, S5, [prop(newline_after_subgoal)]),
    S6 = S5
  ; Context = after_arglist_comma,
    style_option(space_after_arglist_comma(Space_After_Arglist_Comma/_), Opts),
    yes(Space_After_Arglist_Comma),
    Newlines = 0,
    Spaces \= 1 ->
    state_warn(S4, S5, [prop(space_after_arglist_comma), found(Spaces), expected(1)]),
    S6 = S5
  ; otherwise ->
    S6 = S4
  ),
  ( Newlines > 0 ->
    check(Opts, S6, S7, indent)
  ; otherwise ->
    S7 = S6
  ),
  SN = S7.

layout_spaces(_Opts, SN, SN, []).
layout_spaces(Opts, S0, SN, [PT|PTs]) :-
  layout_space(Opts, S0, S1, PT),
  layout_spaces(Opts, S1, SN, PTs).

comment_spaces(_Opts, SN, SN, []).
comment_spaces(Opts, S0, SN, [PT|PTs]) :-
  comment_space(Opts, S0, S1, PT),
  comment_spaces(Opts, S1, SN, PTs).

layout_space(_Opts, S0, SN, layout_text(layout_char(space_char(' ')))) :-
  state_space(S0, S1, cols(1)),
  set_context(S1, S2, layout, prev(space)),
  inc_context(S2, SN, layout, leading_spaces).

layout_space(_Opts, S0, SN, layout_text(layout_char(horizontal_tab_char('\t')))) :-
  state_space(S0, S1, cols(1)),
  set_context(S1, S2, layout, prev(tab)),
  inc_context(S2, SN, layout, leading_tabs).

layout_space(Opts, S0, SN, layout_text(layout_char(new_line_char(_)))) :-
  check(Opts, S0, S2, max_line_length),
  get_context(S0, layout, prev(Before)),
  ( ( Before = space ; Before = space ),
    style_option(no_eol_whitespace(No_EOL_Whitespace/_), Opts),
    yes(No_EOL_Whitespace) ->
    state_warn(S2, S3, [prop(no_eol_whitespace)])
  ; otherwise ->
    S3 = S2
  ),
  inc_context(S3, S4, layout, newlines),
  state_space(S4, S5, rows(1)),
  set_context(S5, S6, layout, prev(newline)),
  % reset leading spaces
  set_context(S6, S7, layout, leading_spaces(0)),
  set_context(S7, SN, layout, leading_tabs(0)).

layout_space(Opts, S0, SN, layout_text(comment(single_line_comment(PTs)))) :-
  PTs = [
    end_line_comment_char('%'),
    comment_text(CT, _),
    new_line_char(_)
  ],
  state_space(S0, S1, cols(1)), % "%" symbol
  atom_length(CT, Length),
  state_space(S1, S2, cols(Length)),
  style_option(max_line_length(_/Secondary), Opts),
  ( option(ignore(comments), Secondary) ->
    S3 = S2
  ; otherwise ->
    check(Opts, S2, S3, max_line_length)
  ),
  inc_context(S3, S4, layout, newlines),
  state_space(S4, S5, rows(1)),
  % reset leading spaces
  set_context(S5, S6, layout, leading_spaces(0)),
  set_context(S6, SN, layout, leading_tabs(0)).

layout_space(Opts, S0, SN, layout_text(comment(bracketed_comment(PTs)))) :-
  PTs = [
    comment_open([
      comment_1_char('/'),
      comment_2_char('*')
    ]),
    comment_text(_CT, PTs_Comment_Text),
    comment_close([
      comment_2_char('*'),
      comment_1_char('/')
    ])
  ],
  comment_spaces(Opts, S0, SN, PTs_Comment_Text).

comment_space(Opts, S0, SN, char(layout_char(new_line_char(_)))) :-
  !,
  style_option(max_line_length(Max_Line_Length/Secondary), Opts),
  ( \+ no(Max_Line_Length),
    \+ option(ignore(comments), Secondary) ->
    check(Opts, S0, S1, max_line_length)
  ; otherwise ->
    S1 = S0
  ),
  state_space(S1, SN, rows(1)).

comment_space(_Opts, S0, SN, char(layout_char(horizontal_tab_char('\t')))) :-
  !,
  %% TODO: Covington 2.1
  state_space(S0, SN, cols(1)).

comment_space(_Opts, S0, SN, char(_)) :-
  state_space(S0, SN, cols(1)).

quoted_items(Opts0, S0, SN, Items, Atom) :-
  % use plammar options as we use phrase for plammar DCG
  normalise_options(prolog_parsetree, Opts0, Opts),
  quoted_space(Opts, S0, SN, Items, Cs-Cs),
  atom_chars(Atom, Cs).

quoted_space(_Opts, S0, S0, [], _-[]).
quoted_space(Opts, S0, SN, [PT|Rest], Cs0-Cs0e) :-
  PT =.. [_X_Quoted_Item, PT_Quoted_Item],
  PT_Quoted_Item =.. [Item, Inner],
  quoted_space_item(Opts, S0, S1, Item, Inner, Cs0e, Cs1e),
  quoted_space(Opts, S1, SN, Rest, Cs0-Cs1e).

quoted_space_item(Opts, S0, SN, Character, Inner, Cs0e, Cs1e) :-
  member(Character, [double_quoted_character, back_quoted_character, single_quoted_character]),
  state_space(S0, S1, cols(1)), % single character
  PT =.. [Character, Inner],
  Callable =.. [Character, Opts, PT, Chars, []],
  call(plammar:Callable),
  append(Chars, Cs1e, Cs0e),
  ( Chars = [Char],
    Char = '\t',
    style_option(tab_in_quotes(Tab_In_Quotes/_), Opts),
    no(Tab_In_Quotes) ->
    state_warn(S1, S2, [prop(tab_in_quotes)])
  ; otherwise ->
    S2 = S1
  ),
  check_entity(Opts, S2, S3, symbolic_chars, Inner),
  check_entity(Opts, S3, S4, missing_closing_backslash_in_character_escape, Inner),
  check_entity(Opts, S4, S5, unicode_character_escape, Inner),
  SN = S5.

quoted_space_item(_Opts, S0, SN, continuation_escape_sequence, _Inner, Cs0e, Cs0e) :-
  state_space(S0, SN, rows(1)).
