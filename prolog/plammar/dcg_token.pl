:- module(lexer, []).

:- op(800, fy, *).
:- op(800, fy, ?).

/* 6.4 TOKENS */

% also allow newlines, etc. at the end of the file
term(Opts) -->
    ?shebang(Opts),
    *token(Opts),
    ?layout_text_sequence(Opts).
/*
term(Opts) -->                      % 6.4
    *token(Opts).                   % 6.4
*/
read_term_(Opts) -->                % 6.4
    term(Opts)                      % 6.4
  , end(Opts).                      % 6.4

% Optimised version to avoid backtracking of
%   layout_text_sequence.
token_(Opts, token_(Tree), A, Z) :-
  ( \+ var(A) ->
    ( layout_text_sequence(Opts, LTS_Tree, A, B),
      Inner = [LTS_Tree, Token_Tree]
    ; A = B,
      Inner = [Token_Tree] )
  ; % otherwise ->
    Tree =.. [_, Inner],
    ( Inner = [LTS_Tree, Token_Tree] ->
      layout_text_sequence(Opts, LTS_Tree, A, B)
    ; Inner = open_token(_) ->
      Inner = Token_Tree,
      A = B
    ; Inner = [Token_Tree] ->
      A = B)
  ),
  ( name_token(Opts, Token_Tree, B, Z),
    Tree = name(Inner)
  ; variable_token(Opts, Token_Tree, B, Z),
    Tree = variable(Inner)
  ; integer_token(Opts, Token_Tree, B, Z),
    Tree = integer(Inner)
  ; float_number_token(Opts, Token_Tree, B, Z),
    Tree = float_number(Inner)
  ; double_quoted_list_token(Opts, Token_Tree, B, Z),
    Tree = double_quoted_list(Inner)
  ; close_token(Token_Tree, B, Z),
    Tree = close(Inner)
  ; open_list_token(Token_Tree, B, Z),
    Tree = open_list(Inner)
  ; close_list_token(Token_Tree, B, Z),
    Tree = close_list(Inner)
  ; open_curly_token(Token_Tree, B, Z),
    Tree = open_curly(Inner)
  ; close_curly_token(Token_Tree, B, Z),
    Tree = close_curly(Inner)
  ; head_tail_separator_token(Token_Tree, B, Z),
    Tree = ht_sep(Inner)
  ; comma_token(Token_Tree, B, Z),
    Tree = comma(Inner)
  ; open_token(Token_Tree, B, Z),
    ( Inner = [LTS_Tree, Token_Tree] ->
      Tree = open(Inner)
    ; % otherwise ->
      Tree = open_ct(Token_Tree)
    )
  ; back_quoted_string_token(Opts, Token_Tree, B, Z),
    Tree = back_quoted_string(Inner)
  ; end_token(Token_Tree, B, Z),
    Tree = end(Inner)
  ).

/*
token_ -->                          % 6.4
    name                            % 6.4
  | variable                        % 6.4
  | integer                         % 6.4
  | float_number                    % 6.4
  | double_quoted_list              % 6.4
  | open_                           % 6.4
  | open_ct                         % 6.4
  | close_                          % 6.4
  | open_list                       % 6.4
  | close_list                      % 6.4
  | open_curly                      % 6.4
  | close_curly                     % 6.4
  | ht_sep                          % 6.4
  | comma.                          % 6.4

name -->                            % 6.4
    ?layout_text_sequence           % 6.4
  , name_token.

variable -->                        % 6.4
    ?layout_text_sequence           % 6.4
  , variable_token.

integer -->                         % 6.4
    ?layout_text_sequence           % 6.4
  , integer_token.                  % 6.4.4

float_number -->                    % 6.4
    ?layout_text_sequence           % 6.4
  , float_number_token.             % 6.4.5

double_quoted_list -->              % 6.4
    ?layout_text_sequence           % 6.4
  , double_quoted_list_token.       % 6.4.6

open_ -->                           % 6.4
    layout_text_sequence            % 6.4
  , open_token.                     % 6.4.8

open_ct -->                         % 6.4
    open_token.                     % 6.4.8

close_ -->                          % 6.4
    ?layout_text_sequence           % 6.4
  , close_token.                    % 6.4.8

open_list -->                       % 6.4
    ?layout_text_sequence           % 6.4
  , open_list_token.                % 6.4.8

close_list -->                      % 6.4
    ?layout_text_sequence           % 6.4
  , close_list_token.               % 6.4.8

open_curly -->                      % 6.4
    ?layout_text_sequence           % 6.4
  , open_curly_token.               % 6.4.8

close_curly -->                     % 6.4
    ?layout_text_sequence           % 6.4
  , close_curly_token.              % 6.4.8

ht_sep -->                          % 6.4
    ?layout_text_sequence           % 6.4
  , head_tail_separator_token.      % 6.4.8

comma -->                           % 6.4
    ?layout_text_sequence           % 6.4
  , comma_token.                    % 6.4.8

back_quoted_string -->              % 6.4
    ?layout_text_sequence           % 6.4
  , back_quoted_string_token.       % 6.4.
*/

end(Opts) -->                       % 6.4
    ?layout_text_sequence(Opts)     % 6.4
  , end_token.                      % 6.4.8

/* 6.4.1 Layout text */

layout_text_sequence(Opts) -->      % 6.4.1
    layout_text(Opts)               % 6.4.1
  , *layout_text(Opts).             % 6.4.1

layout_text(Opts) -->               % 6.4.1
    layout_char                     % 6.5.4
  | comment(Opts).                  % 6.4.1

comment(Opts) -->                   % 6.4.1
    single_line_comment(Opts)       % 6.4.1
  | bracketed_comment(Opts).        % 6.4.1
/*
single_line_comment(Opts0) -->      % 6.4.1
    { merge_options([disallow_chars(['\n'])], Opts0, Opts) }
  , end_line_comment_char           % 6.5.3
  , comment_text(Opts)              % 6.4.1
  , new_line_char.                  % 6.5.4
*/
single_line_comment(Opts0, single_line_comment([ELCC_Tree,CT_Tree,NLC_Tree]), A, Z) :-
  merge_options([disallow_chars(['\n'])], Opts0, Opts),
  end_line_comment_char(ELCC_Tree, A, B),
  comment_text(Opts, CT_Tree, B, C),
  ( (C == Z ; NLC_Tree == end_of_file) ->
    NLC_Tree = end_of_file,
    C = Z
  ; otherwise ->
    new_line_char(NLC_Tree, C, Z)
  ).

bracketed_comment(Opts) -->         % 6.4.1
    comment_open                    % 6.4.1
  , comment_text(Opts)              % 6.4.1
  , comment_close.                  % 6.4.1
    %% TODO: "The comment text of a bracketed comment
    %%   shall not contain the comment close sequence."

comment_open -->                    % 6.4.1
    comment_1_char                  % 6.4.1
  , comment_2_char.                 % 6.4.1

comment_close -->                   % 6.4.1
    comment_2_char                  % 6.4.1
  , comment_1_char.                 % 6.4.1

comment_text(Opts) wrap_text -->    % 6.4.1
    *char(Opts).                    % 6.5

comment_1_char -->                  % 6.4.1
    ['/'].

comment_2_char -->                  % 6.4.1
    ['*'].

/* 6.4.2 Names */

name token Opts -->                 % 6.4.2
    letter_digit_token(Opts)        % 6.4.2
  | graphic_token(Opts)             % 6.4.2
  | quoted_token(Opts)              % 6.4.2
  | semicolon_token                 % 6.4.2
  | cut_token.                      % 6.4.2

letter_digit_token(Opts) -->        % 6.4.2
    small_letter_char(Opts)         % 6.5.2
  , *alphanumeric_char(Opts).       % 6.5.2

graphic_token(Opts) -->             % 6.4.2
    graphic_token_char(Opts)        % 6.4.2
  , *graphic_token_char(Opts).      % 6.4.2
    %% TODO: "A graphic token shall not begin with
    %%   the Character sequence comment_open (6.4.1)."
    %% TODO: "A graphic token shall not be the
    %%   Single Character . (dot) when . is followed
    %%   by a layout_char or single_line_comment."

graphic_token_char(Opts) -->        % 6.4.2
    graphic_char(Opts)              % 6.5.1
  | backslash_char.                 % 6.5.5

quoted_token(Opts) -->              % 6.4.2
    single_quote_char               % 6.5.5
  , *single_quoted_item(Opts)       % 6.4.2
  , single_quote_char.              % 6.5.5

single_quoted_item(Opts) -->        % 6.4.2
    single_quoted_character(Opts)   % 6.4.2.1
  | continuation_escape_sequence.   % 6.4.2

continuation_escape_sequence -->    % 6.4.2
    backslash_char                  % 6.5.5
  , new_line_char.                  % 6.5.4

semicolon_token -->                 % 6.4.2
    semicolon_char.                 % 6.5.3

cut_token -->                       % 6.4.2
    cut_char.                       % 6.5.3

/* 6.4.2.1 */

single_quoted_character(Opts) -->   % 6.4.2.1
    non_quote_char(Opts).           % 6.4.2.1
single_quoted_character(_Opts) -->  % 6.4.2.1
    single_quote_char               % 6.5.5
  , single_quote_char.              % 6.5.5
single_quoted_character(_Opts) -->  % 6.4.2.1
    double_quote_char.              % 6.5.5
single_quoted_character(_Opts) -->  % 6.4.2.1
    back_quote_char.                % 6.5.5

double_quoted_character(Opts) -->   % 6.4.2.1
    non_quote_char(Opts).           % 6.4.2.1
double_quoted_character(_Opts) -->  % 6.4.2.1
    single_quote_char.              % 6.5.5
double_quoted_character(_Opts) -->  % 6.4.2.1
    double_quote_char               % 6.5.5
  , double_quote_char.              % 6.5.5
double_quoted_character(_Opts) -->  % 6.4.2.1
    back_quote_char.                % 6.5.5


back_quoted_character(Opts) -->     % 6.4.2.1
    non_quote_char(Opts).           % 6.4.2.1
back_quoted_character(_Opts) -->    % 6.4.2.1
    single_quote_char.              % 6.5.5
back_quoted_character(_Opts) -->    % 6.4.2.1
    double_quote_char.              % 6.5.5
back_quoted_character(_Opts) -->    % 6.4.2.1
    back_quote_char                 % 6.5.5
  , back_quote_char.                % 6.5.5

/*
non_quote_char(Opts) -->            % 6.4.2.1
    graphic_char(Opts)              % 6.5.1
  | alphanumeric_char(Opts)         % 6.5.2
  | solo_char                       % 6.5.3
  | space_char                      % 6.5.4
  | meta_escape_sequence            % 6.4.2.1
  | control_escape_sequence(Opts)   % 6.4.2.1
  | octal_escape_sequence(Opts)     % 6.4.2.1
  | hexadecimal_escape_sequence(Opts). % 6.4.2.1
*/
non_quote_char(Opts, non_quote_char(PT), A, Z) :-
  ( graphic_char(Opts, PT, A, Z)
  ; alphanumeric_char(Opts, PT, A, Z)
  ; solo_char(PT, A, Z)
  ; space_char(PT, A, Z)
  ; meta_escape_sequence(PT, A, Z)
  ; control_escape_sequence(Opts, PT, A, Z)
  ; octal_escape_sequence(Opts, PT, A, Z)
  ; hexadecimal_escape_sequence(Opts, PT, A, Z)
  ; option(allow_tab_as_quote_char(Allow_Tab_As_Quote_Char), Opts, no),
    yes(Allow_Tab_As_Quote_Char),
    horizontal_tab_char(PT, A, Z)
  ; option(allow_newline_as_quote_char(Allow_Newline_As_Quote_Char), Opts, no),
    yes(Allow_Newline_As_Quote_Char),
    new_line_char(PT, A, Z)
  ; option(allow_unicode_character_escape(Allow_Unicode_Character_Escape), Opts, no),
    yes(Allow_Unicode_Character_Escape),
    unicode_escape_sequence(Opts, PT, A, Z)
  ).

meta_escape_sequence -->            % 6.4.2.1
    backslash_char                  % 6.5.5
  , meta_char.                      % 6.5.5

control_escape_sequence(Opts) -->   % 6.4.2.1
    backslash_char                  % 6.5.5
  , symbolic_control_char(Opts).    % 6.4.2.1

symbolic_control_char(_Opts) -->    % 6.4.2.1
    symbolic_alert_char             % 6.4.2.1
  | symbolic_backspace_char         % 6.4.2.1
  | symbolic_carriage_return_char   % 6.4.2.1
  | symbolic_form_feed_char         % 6.4.2.1
  | symbolic_horizontal_tab_char    % 6.4.2.1
  | symbolic_new_line_char          % 6.4.2.1
  | symbolic_vertical_tab_char.     % 6.4.2.1

symbolic_control_char(Opts, symbolic_control_char(symbolic_no_output_char('c')), ['c'|Z], Z) :-
  option(allow_symbolic_no_output_char_c(Allow_Control_Char), Opts, no),
  yes(Allow_Control_Char).

symbolic_control_char(Opts, symbolic_control_char(symbolic_escape_char('e')), ['e'|Z], Z) :-
  option(allow_symbolic_escape_char_e(Allow_Control_Char), Opts, no),
  yes(Allow_Control_Char).

symbolic_control_char(Opts, symbolic_control_char(symbolic_space_char('s')), ['s'|Z], Z) :-
  option(allow_symbolic_space_char_s(Allow_Control_Char), Opts, no),
  yes(Allow_Control_Char).

symbolic_alert_char -->             % 6.4.2.1
    ['a'].

symbolic_backspace_char -->         % 6.4.2.1
    ['b'].

symbolic_carriage_return_char -->   % 6.4.2.1
    ['r'].

symbolic_form_feed_char -->         % 6.4.2.1
    ['f'].

symbolic_horizontal_tab_char -->    % 6.4.2.1
    ['t'].

symbolic_new_line_char -->          % 6.4.2.1
    ['n'].

symbolic_vertical_tab_char -->      % 6.4.2.1
    ['v'].
/*
octal_escape_sequence(_Opts) -->    % 6.4.2.1
    backslash_char                  % 6.5.5
  , octal_digit_char                % 6.5.2
  , *octal_digit_char               % 6.5.2
  , backslash_char.                 % 6.5.5
*/
octal_escape_sequence(Opts, octal_escape_sequence([PT_Backslash_Char,PT_Octal_Digit_Char|Digits]), A, Z) :-
  backslash_char(PT_Backslash_Char, A, B),
  octal_digit_char(PT_Octal_Digit_Char, B, C),
  octal_escape_sequence_df(Opts, Digits-Digits, C, D, R),
  ( R = [PT_Backslash_Char],
    backslash_char(PT_Backslash_Char, D, Z)
  ; R = [],
    option(allow_missing_closing_backslash_in_character_escape(Allow_Missing_Closing_Backslash_In_Character_Escape), Opts, no),
    yes(Allow_Missing_Closing_Backslash_In_Character_Escape),
    D = Z
  ).

octal_escape_sequence_df(Opts, Ls0-[PT_Octal_Digit_Char|Ls1e], A, Z, R) :-
  octal_digit_char(PT_Octal_Digit_Char, A, B),
  !,
  octal_escape_sequence_df(Opts, Ls0-Ls1e, B, Z, R).
octal_escape_sequence_df(_Opts, _-R, A, A, R).

/*
hexadecimal_escape_sequence(_Opts) --> % 6.4.2.1
    backslash_char                  % 6.5.5
  , symbolic_hexadecimal_char       % 6.4.2.1
  , hexadecimal_digit_char          % 6.5.2
  , *hexadecimal_digit_char         % 6.5.2
  , backslash_char.                 % 6.5.5
*/
hexadecimal_escape_sequence(Opts, hexadecimal_escape_sequence([PT_Backslash_Char,PT_Symbolic_Hexadecimal_Char,PT_Hexadecimal_Digit_Char|Digits]), A, Z) :-
  backslash_char(PT_Backslash_Char, A, B),
  symbolic_hexadecimal_char(PT_Symbolic_Hexadecimal_Char, B, C),
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, C, D),
  hexadecimal_escape_sequence_df(Opts, Digits-Digits, D, E, R),
  ( R = [PT_Backslash_Char],
    backslash_char(PT_Backslash_Char, E, Z)
  ; R = [],
    option(allow_missing_closing_backslash_in_character_escape(Allow_Missing_Closing_Backslash_In_Character_Escape), Opts, no),
    yes(Allow_Missing_Closing_Backslash_In_Character_Escape),
    E = Z
  ).

hexadecimal_escape_sequence_df(Opts, Ls0-[PT_Hexadecimal_Digit_Char|Ls1e], A, Z, R) :-
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, A, B),
  !,
  hexadecimal_escape_sequence_df(Opts, Ls0-Ls1e, B, Z, R).
hexadecimal_escape_sequence_df(_Opts, _-R, A, A, R).



symbolic_hexadecimal_char -->       % 6.4.2.1
    ['x'].

unicode_escape_sequence(_Opts, unicode_escape_sequence([PT_Backslash_Char, PT_Symbolic_Unicode_Char, PT_Hex1, PT_Hex2, PT_Hex3, PT_Hex4]), A, Z) :-
  backslash_char(PT_Backslash_Char, A, B),
  symbolic_unicode4_char(PT_Symbolic_Unicode_Char, B, C),
  hexadecimal_digit_char(PT_Hex1, C, D),
  hexadecimal_digit_char(PT_Hex2, D, E),
  hexadecimal_digit_char(PT_Hex3, E, F),
  hexadecimal_digit_char(PT_Hex4, F, Z).

unicode_escape_sequence(_Opts, unicode_escape_sequence([PT_Backslash_Char, PT_Symbolic_Unicode_Char, PT_Hex1, PT_Hex2, PT_Hex3, PT_Hex4, PT_Hex5, PT_Hex6, PT_Hex7, PT_Hex8]), A, Z) :-
  backslash_char(PT_Backslash_Char, A, B),
  symbolic_unicode8_char(PT_Symbolic_Unicode_Char, B, C),
  hexadecimal_digit_char(PT_Hex1, C, D),
  hexadecimal_digit_char(PT_Hex2, D, E),
  hexadecimal_digit_char(PT_Hex3, E, F),
  hexadecimal_digit_char(PT_Hex4, F, G),
  hexadecimal_digit_char(PT_Hex5, G, H),
  hexadecimal_digit_char(PT_Hex6, H, I),
  hexadecimal_digit_char(PT_Hex7, I, J),
  hexadecimal_digit_char(PT_Hex8, J, Z).

symbolic_unicode8_char -->
    ['U'].

symbolic_unicode4_char -->
    ['u'].


/* 6.4.3 Variables */

variable token Opts -->             % 6.4.3
    anonymous_variable(Opts)        % 6.4.3
  | named_variable(Opts).           % 6.4.3

anonymous_variable(_Opts) -->       % 6.4.3
    variable_indicator_char.        % 6.4.3

named_variable(Opts) -->            % 6.4.3
    variable_indicator_char         % 6.4.3
  , alphanumeric_char(Opts)         % 6.5.2
  , *alphanumeric_char(Opts).       % 6.5.2
/*
named_variable(Opts) -->            % 6.4.3
    capital_letter_char(Opts)       % 6.5.2
  , *alphanumeric_char(Opts).       % 6.5.2
*/

%% handle var_prefix option
named_variable(Opts, named_variable(Tree), A, Z) :-
  option(var_prefix(Var_Prefix), Opts),
  no(Var_Prefix),
  Tree=[Capital_Letter_Tree|Sequence_List],
  capital_letter_char(Opts, Capital_Letter_Tree, A, B),
  call_sequence_ground(sequence(*, alphanumeric_char(Opts), T), T, [], Sequence_List, B, Z).

variable_indicator_char -->         % 6.4.3
    underscore_char.                % 6.5.2


/* 6.4.4 Integer numbers */

integer token Opts -->              % 6.4.3
    integer_constant_(Opts)         % 6.4.4
  | character_code_constant(Opts)   % 6.4.4
  | binary_constant(Opts)           % 6.4.4
  | octal_constant(Opts)            % 6.4.4
  | hexadecimal_constant(Opts).     % 6.4.4

% Wrapper just for SWI 6+ compatibility,
%   to support options allow_digit_groups_with_underscore
%   and allow_digit_groups_with_space
integer_constant_(Opts0, integer_constant(PT), A, Z) :-
  merge_options([is_integer(yes)], Opts0, Opts),
  integer_constant(Opts, integer_constant(PT), A, Z).
/*
integer_constant(_Opts) -->         % 6.4.4
    decimal_digit_char              % 6.5.2
  , *decimal_digit_char.            % 6.5.2
*/
integer_constant(Opts, integer_constant([PT_Decimal_Digit_Char|PT_Rest]), A, Z) :-
  decimal_digit_char(PT_Decimal_Digit_Char, A, B),
  integer_constant_df(Opts, PT_Rest-PT_Rest, B, Z).

integer_constant_df(Opts, Ls0-[PT_Decimal_Digit_Char|Ls1e], A, Z) :-
  decimal_digit_char(PT_Decimal_Digit_Char, A, B),
  !,
  integer_constant_df(Opts, Ls0-Ls1e, B, Z).
integer_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Decimal_Digit_Char|Ls1e], A, Z) :-
  option(is_integer(yes), Opts, no),
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  decimal_digit_char(PT_Decimal_Digit_Char, B, C),
  !,
  integer_constant_df(Opts, Ls0-Ls1e, C, Z).
integer_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Bracketed_Comment, PT_Decimal_Digit_Char|Ls1e], A, Z) :-
  option(is_integer(yes), Opts, no),
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
  decimal_digit_char(PT_Decimal_Digit_Char, C, D),
  !,
  integer_constant_df(Opts, Ls0-Ls1e, D, Z).
integer_constant_df(Opts, Ls0-[PT_Space_Char, PT_Decimal_Digit_Char|Ls1e], A, Z) :-
  option(is_integer(yes), Opts, no),
  space_char(PT_Space_Char, A, B),
  option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
  yes(Allow_Digit_Groups_With_Space),
  decimal_digit_char(PT_Decimal_Digit_Char, B, C),
  !,
  integer_constant_df(Opts, Ls0-Ls1e, C, Z).
integer_constant_df(_Opts, _-[], A, A).


character_code_constant(Opts) -->   % 6.4.4
    ['0']
  , single_quote_char               % 6.5.5
  , single_quoted_character(Opts).  % 6.4.2.1

character_code_constant(Opts, character_code_constant(['0', PT_Single_Quote_Char, single_quoted_character(single_quote_char('\''))]), A, Z) :-
  A = ['0'|B],
  single_quote_char(PT_Single_Quote_Char, B, C),
  option(allow_single_quote_char_in_character_code_constant(Allow_Single_Quote_Char_In_Character_Code_Constant), Opts, no),
  yes(Allow_Single_Quote_Char_In_Character_Code_Constant),
  C = ['\''|Z].
/*
binary_constant(_Opts) -->          % 6.4.4
    binary_constant_indicator       % 6.4.4
  , binary_digit_char               % 6.5.2
  , *binary_digit_char.             % 6.5.2
*/
binary_constant(Opts, binary_constant([PT_Binary_Constant_Indicator, PT_Binary_Digit_Char|PT_Rest]), A, Z) :-
  binary_constant_indicator(PT_Binary_Constant_Indicator, A, B),
  binary_digit_char(PT_Binary_Digit_Char, B, C),
  binary_constant_df(Opts, PT_Rest-PT_Rest, C, Z).

binary_constant_df(Opts, Ls0-[PT_Binary_Digit_Char|Ls1e], A, Z) :-
  binary_digit_char(PT_Binary_Digit_Char, A, B),
  !,
  binary_constant_df(Opts, Ls0-Ls1e, B, Z).
binary_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Binary_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  binary_digit_char(PT_Binary_Digit_Char, B, C),
  !,
  binary_constant_df(Opts, Ls0-Ls1e, C, Z).
binary_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Bracketed_Comment, PT_Binary_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
  binary_digit_char(PT_Binary_Digit_Char, C, D),
  !,
  binary_constant_df(Opts, Ls0-Ls1e, D, Z).
binary_constant_df(Opts, Ls0-[PT_Space_Char, PT_Binary_Digit_Char|Ls1e], A, Z) :-
  space_char(PT_Space_Char, A, B),
  option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
  yes(Allow_Digit_Groups_With_Space),
  binary_digit_char(PT_Binary_Digit_Char, B, C),
  !,
  binary_constant_df(Opts, Ls0-Ls1e, C, Z).
binary_constant_df(_Opts, _-[], A, A).


binary_constant_indicator -->       % 6.4.4
    ['0', 'b'].
/*
octal_constant(_Opts) -->           % 6.4.4
    octal_constant_indicator        % 6.4.4
  , octal_digit_char                % 6.5.2
  , *octal_digit_char.              % 6.5.2
*/
octal_constant(Opts, octal_constant([PT_Octal_Constant_Indicator, PT_Octal_Digit_Char|PT_Rest]), A, Z) :-
  octal_constant_indicator(PT_Octal_Constant_Indicator, A, B),
  octal_digit_char(PT_Octal_Digit_Char, B, C),
  octal_constant_df(Opts, PT_Rest-PT_Rest, C, Z).

octal_constant_df(Opts, Ls0-[PT_Octal_Digit_Char|Ls1e], A, Z) :-
  octal_digit_char(PT_Octal_Digit_Char, A, B),
  !,
  octal_constant_df(Opts, Ls0-Ls1e, B, Z).
octal_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Octal_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  octal_digit_char(PT_Octal_Digit_Char, B, C),
  !,
  octal_constant_df(Opts, Ls0-Ls1e, C, Z).
octal_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Bracketed_Comment, PT_Octal_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
  octal_digit_char(PT_Octal_Digit_Char, C, D),
  !,
  octal_constant_df(Opts, Ls0-Ls1e, D, Z).
octal_constant_df(Opts, Ls0-[PT_Space_Char, PT_Octal_Digit_Char|Ls1e], A, Z) :-
  space_char(PT_Space_Char, A, B),
  option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
  yes(Allow_Digit_Groups_With_Space),
  octal_digit_char(PT_Octal_Digit_Char, B, C),
  !,
  octal_constant_df(Opts, Ls0-Ls1e, C, Z).
octal_constant_df(_Opts, _-[], A, A).

octal_constant_indicator -->        % 6.4.4
    ['0', 'o'].
/*
hexadecimal_constant(_Opts) -->     % 6.4.4
    hexadecimal_constant_indicator  % 6.4.4
  , hexadecimal_digit_char          % 6.5.2
  , *hexadecimal_digit_char.        % 6.5.2
*/
hexadecimal_constant(Opts, hexadecimal_constant([PT_Hexadecimal_Constant_Indicator, PT_Hexadecimal_Digit_Char|PT_Rest]), A, Z) :-
  hexadecimal_constant_indicator(PT_Hexadecimal_Constant_Indicator, A, B),
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, B, C),
  hexadecimal_constant_df(Opts, PT_Rest-PT_Rest, C, Z).

hexadecimal_constant_df(Opts, Ls0-[PT_Hexadecimal_Digit_Char|Ls1e], A, Z) :-
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, A, B),
  !,
  hexadecimal_constant_df(Opts, Ls0-Ls1e, B, Z).
hexadecimal_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Hexadecimal_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, B, C),
  !,
  hexadecimal_constant_df(Opts, Ls0-Ls1e, C, Z).
hexadecimal_constant_df(Opts, Ls0-[PT_Underscore_Char, PT_Bracketed_Comment, PT_Hexadecimal_Digit_Char|Ls1e], A, Z) :-
  underscore_char(PT_Underscore_Char, A, B),
  option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
  yes(Allow_Digit_Groups_With_Underscore),
  bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, C, D),
  !,
  hexadecimal_constant_df(Opts, Ls0-Ls1e, D, Z).
hexadecimal_constant_df(Opts, Ls0-[PT_Space_Char, PT_Hexadecimal_Digit_Char|Ls1e], A, Z) :-
  space_char(PT_Space_Char, A, B),
  option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
  yes(Allow_Digit_Groups_With_Space),
  hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, B, C),
  !,
  hexadecimal_constant_df(Opts, Ls0-Ls1e, C, Z).
hexadecimal_constant_df(_Opts, _-[], A, A).

hexadecimal_constant_indicator -->  % 6.4.4
    ['0', 'x'].

/* 6.4.5 Floating point numbers */
/*
float_number token Opts -->         % 6.4.5
    integer_constant(Opts)          % 6.4.4
  , fraction                        % 6.4.5
  , ?exponent(Opts).                % 6.4.5
*/
float_number_token(Opts, float_number_token(Atom, [PT_Integer_Constant|G]), A, Z) :-
  integer_constant(Opts, PT_Integer_Constant, A, B),
  option(allow_integer_exponential_notation(Allow_Integer_Exponential_Notation), Opts, no),
  ( yes(Allow_Integer_Exponential_Notation) ->
    call_sequence_ground(sequence('?', fraction, F), F, J, G, B, C)
  ; otherwise ->
    G = [PT_Fraction|J],
    fraction(PT_Fraction, B, C)
  ),
  call_sequence_ground(sequence('?', exponent(Opts), I), I, [], J, C, Z),
  ( var(Atom) ->
    append(Chars, Z, A),
    atom_chars(Atom, Chars)
  ; true
  ).

fraction -->                        % 6.4.5
    decimal_point_char              % 6.4.5
  , decimal_digit_char              % 6.5.2
  , *decimal_digit_char.            % 6.5.2

exponent(Opts) -->                  % 6.4.5
    exponent_char                   % 6.4.5
  , sign                            % 6.4.5
  , integer_constant(Opts).         % 6.4.4

sign -->                            % 6.4.5
    negative_sign_char.             % 6.4.5
sign -->                            % 6.4.5
    ?positive_sign_char.            % 6.4.5

positive_sign_char -->              % 6.4.5
    ['+'].

negative_sign_char -->              % 6.4.5
    ['-'].

decimal_point_char -->              % 6.4.5
    ['.'].

exponent_char -->                   % 6.4.5
    ['e']
  | ['E'].

/* 6.4.6 Double quoted lists */

double_quoted_list token Opts -->   % 6.4.6
    double_quote_char               % 6.5.5
  , *double_quoted_item(Opts)       % 6.4.6
  , double_quote_char.              % 6.5.5

double_quoted_item(Opts) -->        % 6.4.6
    double_quoted_character(Opts)   % 6.4.2.1
  | continuation_escape_sequence.   % 6.4.2

/* 6.4.7 Back quoted strings */

back_quoted_string token Opts -->   % 6.4.7
    back_quote_char                 % 6.5.5
  , *back_quoted_item(Opts)         % 6.4.7
  , back_quote_char.                % 6.5.5

back_quoted_item(Opts) -->          % 6.4.7
    back_quoted_character(Opts)     % 6.4.2.1
  | continuation_escape_sequence.   % 6.4.2

/* 6.4.8 Other tokens */

open_token -->                      % 6.4.8
    open_char.                      % 6.5.3

close_token -->                     % 6.4.8
    close_char.                     % 6.5.3

open_list_token -->                 % 6.4.8
    open_list_char.                 % 6.5.3

close_list_token -->                % 6.4.8
    close_list_char.                % 6.5.3

open_curly_token -->                % 6.4.8
    open_curly_char.                % 6.5.3

close_curly_token -->               % 6.4.8
    close_curly_char.               % 6.5.3

head_tail_separator_token -->       % 6.4.8
    head_tail_separator_char.       % 6.5.3

comma_token -->                     % 6.4.8
    comma_char.                     % 6.5.3

end_token -->                       % 6.4.8
    end_char.                       % 6.4.8

end_char -->                        % 6.4.8
    ['.'].

/* 6.5 PROCESSOR CHARACTER SET */

char(Opts, char(Tree), A, Z) :-
  char_(Opts, char_(Tree), A, Z),
  ( A = [C|Z] ->
    option(disallow_chars(Disallowed), Opts, []),
    \+ member(C, Disallowed)
  ; otherwise ->
    true
  ).

char_(Opts) -->                     % 6.5
    graphic_char(Opts)              % 6.5.1
  | alphanumeric_char(Opts)         % 6.5.2
  | solo_char                       % 6.5.3
  | layout_char                     % 6.5.4
  | meta_char.                      % 6.5.5

/* 6.5.1 Graphic characters */

graphic_char_(_Opts) -->             % 6.5.1
    ['#']
  | ['$']
  | ['&']
  | ['*']
  | ['+']
  | ['-']
  | ['.']
  | ['/']
  | [':']
  | ['<']
  | ['=']
  | ['>']
  | ['?']
  | ['@']
  | ['^']
  | ['~'].

graphic_char(Opts, graphic_char(Char), [Char|Z], Z) :-
  ( graphic_char_(Opts, graphic_char_(Char), [Char|Z], Z),
    !
  ; option(allow_unicode(Allow_Unicode), Opts, no),
    yes(Allow_Unicode),
    Char \= '\\',
    char_type(Char, prolog_symbol) ).

/* 6.5.2 Alphanumeric characters */

alphanumeric_char(Opts) -->         % 6.5.2
    alpha_char(Opts)                % 6.5.2
  | decimal_digit_char.             % 6.5.2

alpha_char(Opts) -->                % 6.5.2
    underscore_char                 % 6.5.2
  | letter_char(Opts).              % 6.5.2

letter_char(Opts) -->               % 6.5.2
    capital_letter_char(Opts)       % 6.5.2
  | small_letter_char(Opts).        % 6.5.2

small_letter_char_(_Opts) -->       % 6.5.2
    ['a']
  | ['b']
  | ['c']
  | ['d']
  | ['e']
  | ['f']
  | ['g']
  | ['h']
  | ['i']
  | ['j']
  | ['k']
  | ['l']
  | ['m']
  | ['n']
  | ['o']
  | ['p']
  | ['q']
  | ['r']
  | ['s']
  | ['t']
  | ['u']
  | ['v']
  | ['w']
  | ['x']
  | ['y']
  | ['z'].

small_letter_char(Opts, small_letter_char(Char), [Char|Z], Z) :-
  ( small_letter_char_(Opts, small_letter_char_(Char), [Char|Z], Z),
    !
  ; option(allow_unicode(Allow_Unicode), Opts, no),
    yes(Allow_Unicode),
    char_type(Char, lower) ).

capital_letter_char_(_Opts) -->     % 6.5.2
    ['A']
  | ['B']
  | ['C']
  | ['D']
  | ['E']
  | ['F']
  | ['G']
  | ['H']
  | ['I']
  | ['J']
  | ['K']
  | ['L']
  | ['M']
  | ['N']
  | ['O']
  | ['P']
  | ['Q']
  | ['R']
  | ['S']
  | ['T']
  | ['U']
  | ['V']
  | ['W']
  | ['X']
  | ['Y']
  | ['Z'].

capital_letter_char(Opts, capital_letter_char(Char), [Char|Z], Z) :-
  ( capital_letter_char_(Opts, capital_letter_char_(Char), [Char|Z], Z),
    !
  ; option(allow_unicode(Allow_Unicode), Opts, no),
    yes(Allow_Unicode),
    char_type(Char, upper) ).

decimal_digit_char -->              % 6.5.2
    ['0']
  | ['1']
  | ['2']
  | ['3']
  | ['4']
  | ['5']
  | ['6']
  | ['7']
  | ['8']
  | ['9'].

binary_digit_char -->               % 6.5.2
    ['0']
  | ['1'].

octal_digit_char -->                % 6.5.2
    ['0']
  | ['1']
  | ['2']
  | ['3']
  | ['4']
  | ['5']
  | ['6']
  | ['7'].

hexadecimal_digit_char -->          % 6.5.2
    ['0']
  | ['1']
  | ['2']
  | ['3']
  | ['4']
  | ['5']
  | ['6']
  | ['7']
  | ['8']
  | ['9']
  | ['A']
  | ['a']
  | ['B']
  | ['b']
  | ['C']
  | ['c']
  | ['D']
  | ['d']
  | ['E']
  | ['e']
  | ['F']
  | ['f'].

underscore_char -->                 % 6.5.2
    ['_'].

/* 6.5.3 Solo characters */

solo_char -->                       % 6.5.3
    cut_char                        % 6.5.3
  | open_char                       % 6.5.3
  | close_char                      % 6.5.3
  | comma_char                      % 6.5.3
  | semicolon_char                  % 6.5.3
  | open_list_char                  % 6.5.3
  | close_list_char                 % 6.5.3
  | open_curly_char                 % 6.5.3
  | close_curly_char                % 6.5.3
  | head_tail_separator_char        % 6.5.3
  | end_line_comment_char.          % 6.5.3

cut_char -->                        % 6.5.3
    ['!'].

open_char -->                       % 6.5.3
    ['('].

close_char -->                      % 6.5.3
    [')'].

comma_char -->                      % 6.5.3
    [','].

semicolon_char -->                  % 6.5.3
    [';'].

open_list_char -->                  % 6.5.3
    ['['].

close_list_char -->                 % 6.5.3
    [']'].

open_curly_char -->                 % 6.5.3
    ['{'].

close_curly_char -->                % 6.5.3
    ['}'].

head_tail_separator_char -->        % 6.5.3
    ['|'].

end_line_comment_char -->           % 6.5.3
    ['%'].


/* 6.5.4 Layout characters */

layout_char -->                     % 6.5.4
    space_char                      % 6.5.4
  | horizontal_tab_char             % 6.5.4
  | new_line_char.                  % 6.5.4

space_char -->                      % 6.5.4
    [' '].

horizontal_tab_char -->             % 6.5.4
    ['\t'].                         % implementation dependent

new_line_char -->                   % 6.5.4
    ['\n']                          % implementation dependent
  | ['\r','\n'].

/* 6.5.5 Meta characters */

meta_char -->                       % 6.5.5
    backslash_char                  % 6.5.5
  | single_quote_char               % 6.5.5
  | double_quote_char               % 6.5.5
  | back_quote_char.                % 6.5.5

backslash_char -->                  % 6.5.5
    ['\\'].

single_quote_char -->               % 6.5.5
    ['\''].

double_quote_char -->               % 6.5.5
    ['"'].

back_quote_char -->                 % 6.5.5
    ['`'].


/* SWI Compatibility */

shebang(Opts0, shebang(['#','!',PT_Comment_Text,PT_New_Line_Char]), ['#','!'|A], Z) :-
  option(allow_shebang(Allow_Shebang), Opts0, no),
  yes(Allow_Shebang),
  merge_options([disallow_chars(['\n'])], Opts0, Opts),
  comment_text(Opts, PT_Comment_Text, A, B),
  ( (B == Z ; NLC_Tree == end_of_file) ->
    NLC_Tree = end_of_file,
    B = Z
  ; otherwise ->
    new_line_char(PT_New_Line_Char, B, Z)
  ).
