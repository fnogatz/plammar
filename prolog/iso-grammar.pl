%% prolog//0 is the entry point for the library's
%%   interfaces.
prolog -->
    variable_token.

/* 6.3 TERMS */

/* 6.3.1 Atomic terms */

/* 6.3.1.1 Numbers */
/*
term -->
    integer.                        % 6.4.4 

term -->
    float_number.                   % 6.4.5
*/
/* 6.3.1.2 Negative Numbers */
/*
term -->
    ['-']
  , integer.                        % 6.4.4

term -->
    ['-']
  , float_number.                   % 6.4.5
*/
/* 6.3.1.3 Atoms */

%% TODO: 6.3.1.3

/* 6.3.2 Variables */
/*
term -->
    variable.                       % 6.4.3
*/

/* 6.4 TOKENS */

term -->                            % 6.4
    *token.                         % 6.4

%% TODO: remove suffix
read_term_  -->                     % 6.4
    term                            % 6.4
  , end.                            % 6.4

token_ -->                          % 6.4
    name                            % 6.4
  | variable                        % 6.4
  | integer                         % 6.4
  | float_number                    % 6.4
  | double_quoted_list              % 6.4
  | open                            % 6.4
  | open_ct                         % 6.4
  | close_                          % 6.4  %% TODO: remove suffix
  | open_list                       % 6.4
  | close_list                      % 6.4
  | open_curly                      % 6.4
  | close_curly                     % 6.4
  | ht_sep                          % 6.4
  | comma.                          % 6.4

name -->                            % 6.4
    ?layout_text_sequence           % 6.4.1
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

open -->                            % 6.4
    ?layout_text_sequence           % 6.4
  , open_token.                     % 6.4.8

open_ct -->                         % 6.4
    ?layout_text_sequence           % 6.4
  , open_token.                     % 6.4.8

%% TODO: remove suffix
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

end -->                             % 6.4
    ?layout_text_sequence           % 6.4
  , end_token.                      % 6.4.8

/* 6.4.1 Layout text */

layout_text_sequence -->            % 6.4.1
    layout_text                     % 6.4.1
  , *layout_text.                   % 6.4.1

layout_text -->                     % 6.4.1
    layout_char                     % 6.5.4
  | comment.                        % 6.4.1

comment -->                         % 6.4.1
    single_line_comment             % 6.4.1
  | bracketed_comment.              % 6.4.1

single_line_comment -->             % 6.4.1
    end_line_comment_char           % 6.5.3
  , comment_text                    % 6.4.1
  , new_line_char.                  % 6.5.4
    %% TODO: "The comment text of a Single line
    %%   comment shall not contain a new line char."

bracketed_comment -->               % 6.4.1
    comment_open                    % 6.4.1
  , comment_text                    % 6.4.1
  , comment_close.                  % 6.4.1
    %% TODO: "The comment text of a bracketed comment
    %%   shall not contain the comment close sequence."

comment_open -->                    % 6.4.1
    comment_1_char                  % 6.4.1
  , comment_2_char.                 % 6.4.1

comment_close -->                   % 6.4.1
    comment_2_char                  % 6.4.1
  , comment_1_char.                 % 6.4.1

comment_text -->                    % 6.4.1
    *char.                          % 6.5

comment_1_char -->                  % 6.4.1
    ['/'].

comment_2_char -->                  % 6.4.1
    ['*'].

/* 6.4.2 Names */

name_token -->                      % 6.4.2
    letter_digit_token              % 6.4.2
  | graphic_token                   % 6.4.2
  | quoted_token                    % 6.4.2
  | semicolon_token                 % 6.4.2
  | cut_token.                      % 6.4.2

letter_digit_token -->              % 6.4.2
    small_letter_char               % 6.5.2
  , *alphanumeric_char.             % 6.5.2

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

double_quoted_char -->              % 6.4.2.1
    non_quote_char.                 % 6.4.2.1
double_quoted_char -->              % 6.4.2.1
    single_quote_char.              % 6.5.5
double_quoted_char -->              % 6.4.2.1
    double_quote_char               % 6.5.5
  , double_quote_char.              % 6.5.5
double_quoted_char -->              % 6.4.2.1
    back_quote_char.                % 6.5.5


back_quoted_char -->                % 6.4.2.1
    non_quote_char.                 % 6.4.2.1
back_quoted_char -->                % 6.4.2.1
    single_quote_char.              % 6.5.5
back_quoted_char -->                % 6.4.2.1
    double_quote_char.              % 6.5.5
back_quoted_char -->                % 6.4.2.1
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

variable_token -->                  % 6.4.3
    anonymous_variable              % 6.4.3
  | named_variable.                 % 6.4.3

anonymous_variable -->              % 6.4.3
    variable_indicator_char.        % 6.4.3

named_variable -->                  % 6.4.3
    variable_indicator_char         % 6.4.3
  , alphanumeric_char               % 6.5.2
  , *alphanumeric_char.             % 6.5.2
named_variable -->                  % 6.4.3
    capital_letter_char             % 6.5.2
  , *alphanumeric_char.             % 6.5.2

variable_indicator_char -->         % 6.4.3
    underscore_char.                % 6.5.2


/* 6.4.4 Integer numbers */

integer_token -->                   % 6.4.3
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

float_number_token -->              % 6.4.5
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

double_quoted_list_token -->        % 6.4.6
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

/* 6.5 Processor character set */

char -->                            % 6.5
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
    ['\n'].                         % implementation dependent

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
