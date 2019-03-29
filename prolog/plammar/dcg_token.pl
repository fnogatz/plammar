:- module(lexer, []).

:- op(800, fy, *).
:- op(800, fy, ?).

/* 6.4 TOKENS */

% also allow newlines, etc. at the end of the file
term(Opts) -->
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
      Tree = open_(Inner)
    ; % otherwise ->
      Tree = open_ct(Token_Tree)
    )
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

single_line_comment(Opts0) -->      % 6.4.1
    { merge_options([disallow_chars(['\n'])], Opts0, Opts) }
  , end_line_comment_char           % 6.5.3
  , comment_text(Opts)              % 6.4.1
  , new_line_char.                  % 6.5.4

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

comment_text(Opts) -->              % 6.4.1
    *char(Opts).                    % 6.5

comment_1_char -->                  % 6.4.1
    ['/'].

comment_2_char -->                  % 6.4.1
    ['*'].

/* 6.4.2 Names */

name token Opts -->                 % 6.4.2
    letter_digit_token(Opts)        % 6.4.2
  | graphic_token                   % 6.4.2
  | quoted_token                    % 6.4.2
  | semicolon_token                 % 6.4.2
  | cut_token.                      % 6.4.2

/*
letter_digit_token(_Opts) -->       % 6.4.2
    small_letter_char               % 6.5.2
  , *alphanumeric_char.             % 6.5.2
*/
letter_digit_token(Opts, letter_digit_token(Tree), A, Z) :-
  Tree=[Char_Tree|Alphanumeric_Char_List],
  ( small_letter_char(Char_Tree, A, B)
  ; option(var_prefix(Var_Prefix), Opts),
    yes(Var_Prefix),
    capital_letter_char(Char_Tree, A, B) ),
  call_sequence_ground(sequence(*, alphanumeric_char, E), E, [], Alphanumeric_Char_List, B, Z).

graphic_token -->                   % 6.4.2
    graphic_token_char              % 6.4.2
  , *graphic_token_char.            % 6.4.2
    %% TODO: "A graphic token shall not begin with
    %%   the Character sequence comment_open (6.4.1)."
    %% TODO: "A graphic token shall not be the
    %%   Single Character . (dot) when . is followed
    %%   by a layout_char or single_line_comment."

graphic_token_char -->              % 6.4.2
    graphic_char                    % 6.5.1
  | backslash_char.                 % 6.5.5

quoted_token -->                    % 6.4.2
    single_quote_char               % 6.5.5
  , *single_quoted_item             % 6.4.2
  , single_quote_char.              % 6.5.5

single_quoted_item -->              % 6.4.2
    single_quoted_character         % 6.4.2.1
  | continuation_escape_sequence.   % 6.4.2

continuation_escape_sequence -->    % 6.4.2
    backslash_char                  % 6.5.5
  , new_line_char.                  % 6.5.4

semicolon_token -->                 % 6.4.2
    semicolon_char.                 % 6.5.3

cut_token -->                       % 6.4.2
    cut_char.                       % 6.5.3

/* 6.4.2.1 */

single_quoted_character -->         % 6.4.2.1
    non_quote_char.                 % 6.4.2.1
single_quoted_character -->         % 6.4.2.1
    single_quote_char               % 6.5.5
  , single_quote_char.              % 6.5.5
single_quoted_character -->         % 6.4.2.1
    double_quote_char.              % 6.5.5
single_quoted_character -->         % 6.4.2.1
    back_quote_char.                % 6.5.5

double_quoted_character -->         % 6.4.2.1
    non_quote_char.                 % 6.4.2.1
double_quoted_character -->         % 6.4.2.1
    single_quote_char.              % 6.5.5
double_quoted_character -->         % 6.4.2.1
    double_quote_char               % 6.5.5
  , double_quote_char.              % 6.5.5
double_quoted_character -->         % 6.4.2.1
    back_quote_char.                % 6.5.5


back_quoted_character -->           % 6.4.2.1
    non_quote_char.                 % 6.4.2.1
back_quoted_character -->           % 6.4.2.1
    single_quote_char.              % 6.5.5
back_quoted_character -->           % 6.4.2.1
    double_quote_char.              % 6.5.5
back_quoted_character -->           % 6.4.2.1
    back_quote_char                 % 6.5.5
  , back_quote_char.                % 6.5.5

non_quote_char -->                  % 6.4.2.1
    graphic_char                    % 6.5.1
  | alphanumeric_char               % 6.5.2
  | solo_char                       % 6.5.3
  | space_char                      % 6.5.4
  | meta_escape_sequence            % 6.4.2.1
  | control_escape_sequence         % 6.4.2.1
  | octal_escape_sequence           % 6.4.2.1
  | hexadecimal_escape_sequence.    % 6.4.2.1

meta_escape_sequence -->            % 6.4.2.1
    backslash_char                  % 6.5.5
  , meta_char.                      % 6.5.5

control_escape_sequence -->         % 6.4.2.1
    backslash_char                  % 6.5.5
  , symbolic_control_char.          % 6.4.2.1

symbolic_control_char -->           % 6.4.2.1
    symbolic_alert_char             % 6.4.2.1
  | symbolic_backspace_char         % 6.4.2.1
  | symbolic_carriage_return_char   % 6.4.2.1
  | symbolic_form_feed_char         % 6.4.2.1
  | symbolic_horizontal_tab_char    % 6.4.2.1
  | symbolic_new_line_char          % 6.4.2.1
  | symbolic_vertical_tab_char.     % 6.4.2.1

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

octal_escape_sequence -->           % 6.4.2.1
    backslash_char                  % 6.5.5
  , octal_digit_char                % 6.5.2
  , *octal_digit_char               % 6.5.2
  , backslash_char.                 % 6.5.5

hexadecimal_escape_sequence -->     % 6.4.2.1
    backslash_char                  % 6.5.5
  , symbolic_hexadecimal_char       % 6.4.2.1
  , hexadecimal_digit_char          % 6.5.2
  , *hexadecimal_digit_char         % 6.5.2
  , backslash_char.                 % 6.5.5

symbolic_hexadecimal_char -->       % 6.4.2.1
    ['x'].

/* 6.4.3 Variables */

variable token Opts -->             % 6.4.3
    anonymous_variable(Opts)        % 6.4.3
  | named_variable(Opts).           % 6.4.3

anonymous_variable(_Opts) -->       % 6.4.3
    variable_indicator_char.        % 6.4.3

named_variable(_Opts) -->           % 6.4.3
    variable_indicator_char         % 6.4.3
  , alphanumeric_char               % 6.5.2
  , *alphanumeric_char.             % 6.5.2
/*
named_variable(_Opts) -->           % 6.4.3
    capital_letter_char             % 6.5.2
  , *alphanumeric_char.             % 6.5.2
*/

%% handle var_prefix option
named_variable(Opts, named_variable(Tree), A, Z) :-
  option(var_prefix(Var_Prefix), Opts),
  no(Var_Prefix),
  Tree=[Capital_Letter_Tree|Sequence_List],
  capital_letter_char(Capital_Letter_Tree, A, B),
  call_sequence_ground(sequence(*, alphanumeric_char, T), T, [], Sequence_List, B, Z).

variable_indicator_char -->         % 6.4.3
    underscore_char.                % 6.5.2


/* 6.4.4 Integer numbers */

integer token _Opts -->             % 6.4.3
    integer_constant                % 6.4.4
  | character_code_constant         % 6.4.4
  | binary_constant                 % 6.4.4
  | octal_constant                  % 6.4.4
  | hexadecimal_constant.           % 6.4.4

integer_constant -->                % 6.4.4
    decimal_digit_char              % 6.5.2
  , *decimal_digit_char.            % 6.5.2

character_code_constant -->         % 6.4.4
    ['0']
  , single_quote_char               % 6.5.5
  , single_quoted_character.        % 6.4.2.1

binary_constant -->                 % 6.4.4
    binary_constant_indicator       % 6.4.4
  , binary_digit_char               % 6.5.2
  , *binary_digit_char.             % 6.5.2

binary_constant_indicator -->       % 6.4.4
    ['0', 'b'].

octal_constant -->                  % 6.4.4
    octal_constant_indicator        % 6.4.4
  , octal_digit_char                % 6.5.2
  , *octal_digit_char.              % 6.5.2

octal_constant_indicator -->        % 6.4.4
    ['0', 'o'].

hexadecimal_constant -->            % 6.4.4
    hexadecimal_constant_indicator  % 6.4.4
  , hexadecimal_digit_char          % 6.5.2
  , *hexadecimal_digit_char.        % 6.5.2

hexadecimal_constant_indicator -->  % 6.4.4
    ['0', 'x'].

/* 6.4.5 Floating point numbers */

float_number token _Opts -->        % 6.4.5
    integer_constant                % 6.4.4
  , fraction                        % 6.4.5
  , ?exponent.                      % 6.4.5

fraction -->                        % 6.4.5
    decimal_point_char              % 6.4.5
  , decimal_digit_char              % 6.5.2
  , *decimal_digit_char.            % 6.5.2

exponent -->                        % 6.4.5
    exponent_char                   % 6.4.5
  , sign                            % 6.4.5
  , integer_constant.               % 6.4.4

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

double_quoted_list token _Opts -->  % 6.4.6
    double_quote_char               % 6.5.5
  , *double_quoted_item             % 6.4.6
  , double_quote_char.              % 6.5.5

double_quoted_item -->              % 6.4.6
    double_quoted_character         % 6.4.2.1
  | continuation_escape_sequence.   % 6.4.2

/* 6.4.7 Back quoted strings */

back_quoted_string -->              % 6.4.7
    back_quote_char                 % 6.5.5
  , *back_quoted_item               % 6.4.7
  , back_quote_char.                % 6.5.5

back_quoted_item -->                % 6.4.7
    back_quoted_character           % 6.4.2.1
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

char(Opts, char(Tree), [C|Z], Z) :-
  char_(char_(Tree), [C|Z], Z),
  option(disallow_chars(Disallowed), Opts, []),
  \+ member(C, Disallowed).
%% TODO: new_line_char could consume two elements

char_ -->                           % 6.5
    graphic_char                    % 6.5.1
  | alphanumeric_char               % 6.5.2
  | solo_char                       % 6.5.3
  | layout_char                     % 6.5.4
  | meta_char.                      % 6.5.5

/* 6.5.1 Graphic characters */

graphic_char -->                    % 6.5.1
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

/* 6.5.2 Alphanumeric characters */

alphanumeric_char -->               % 6.5.2
    alpha_char                      % 6.5.2
  | decimal_digit_char.             % 6.5.2

alpha_char -->                      % 6.5.2
    underscore_char                 % 6.5.2
  | letter_char.                    % 6.5.2

letter_char -->                     % 6.5.2
    capital_letter_char             % 6.5.2
  | small_letter_char.              % 6.5.2

small_letter_char -->               % 6.5.2
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

capital_letter_char -->             % 6.5.2
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
