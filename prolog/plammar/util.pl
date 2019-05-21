:- module(plammar_util, [
    warning/1,
    warning/2,
    list_open/2,
    list_close/1,
    yes/1,
    no/1,
    spec_class/2,
    set_option/3,
    integer_number/3,
    use_msg/3,
    use_msg/4
  ]).

warning(Format, Arguments) :-
  print_message(warning, format(Format, Arguments)).
warning(Msg) :-
  warning(Msg, []).

list_open(List, Open_List) :-
  append(List, _, Open_List).

list_close([]).
list_close([_|Xs]) :-
  ( var(Xs) -> Xs = []
  ; list_close(Xs) ).

yes(yes).
yes(true).
yes(y).
yes(ok).

no(no).
no(false).
no(n).

spec_class( fx, prefix).
spec_class( fy, prefix).
spec_class(xfx, infix).
spec_class(xfy, infix).
spec_class(yfx, infix).
spec_class(xf , postfix).
spec_class(yf , postfix).

set_option(New_Option, Old, New) :-
  merge_options([New_Option], Old, New).

character_code_integer('a', 7).
character_code_integer('b', 8).
character_code_integer('c', 99).
character_code_integer('e', 27).
character_code_integer('f', 12).
character_code_integer('n', 10).
character_code_integer('r', 13).
character_code_integer('s', 32).
character_code_integer('t', 9).
character_code_integer('v', 11).

integer_number(_Atom, integer_constant(Chars), Integer) :-
  chars2dec(Chars, 10, 0, Integer).

integer_number(_Atom, character_code_constant(['0', single_quote_char('\''), single_quoted_character(non_quote_char( control_escape_sequence([backslash_char('\\'), symbolic_control_char(Symbolic_Control_Char)])))]), Integer) :-
  Symbolic_Control_Char =.. [_Type, Char],
  character_code_integer(Char, Integer).

integer_number(Atom, character_code_constant(_), Integer) :-
  atom_concat('0\'', Char, Atom),
  atom_codes(Char, Codes),
  Codes = [Integer].

integer_number(_Atom, binary_constant([binary_constant_indicator(['0','b'])|Chars]), Integer) :-
  chars2dec(Chars, 2, 0, Integer).

integer_number(_Atom, octal_constant([octal_constant_indicator(['0','o'])|Chars]), Integer) :-
  chars2dec(Chars, 8, 0, Integer).

integer_number(_Atom, hexadecimal_constant([hexadecimal_constant_indicator(['0','x'])|Chars]), Integer) :-
  chars2dec(Chars, 16, 0, Integer).

chars2dec([], _, N, N).
chars2dec([space_char(' ')|Chars], Base, Acc, N) :-
  !,
  chars2dec(Chars, Base, Acc, N).
chars2dec([underscore_char('_')|Chars], Base, Acc, N) :-
  !,
  chars2dec(Chars, Base, Acc, N).
chars2dec([PT|Chars], Base, Acc, N) :-
  PT =.. [_Type, Char],
  code_type(Char, xdigit(Digit)),
  Acc1 is Acc*Base + Digit,
  chars2dec(Chars, Base, Acc1, N).

use_msg(Found, Expected, Msg) :-
  format(atom(Msg), 'Use "~w" instead of "~w".', [Expected, Found]).

use_msg(Found, Expected, Msg, Why) :-
  format(atom(Msg), 'Use "~w" instead of "~w" ~w.', [Expected, Found, Why]).
