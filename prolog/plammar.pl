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
%  phrase(plammar:term(Options, term(Tokens)), Chars, []).
  tokens(Options, Tokens, Chars).

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


:- discontiguous tokens/4, tokens/5.

test_tokens(file(File), Tokens, Opts) :-
  open(File, read, Stream),
  read_string(Stream, _Length, String),
  string_chars(String, Chars),
  tokens(Opts, Tokens, Chars).

tokens(Opts, Tokens, A) :-
  \+ var(Tokens),
  !,
  phrase(plammar:term(Opts, term(Tokens)), A, []).

tokens(Opts, Tokens, A) :-
  var(Tokens),
  !,
  tokens(Opts, start, Tokens, A, []),
  !.

%% start
tokens(Opts, start, Tokens, A, LTS0) :-
  ( A = [] ->
    ( LTS0 = [] ->
      Tokens = []
    ; otherwise ->
      Tokens = [layout_text_sequence(LTS0)]
    )
  ; layout_char(PT_Layout_Char, A, B) ->
    append(LTS0, [layout_text(PT_Layout_Char)], LTS1),
    tokens(Opts, start, Tokens, B, LTS1)
  ; comment_open(PT_Comment_Open, A, B) ->
    tokens(Opts, bracketed_comment(LTS0,[],B), Tokens, PT_Comment_Open, B)
  ; end_line_comment_char(PT_End_Line_Comment_Char, A, B) ->
    tokens(Opts, single_line_comment(LTS0,[],B), Tokens, PT_End_Line_Comment_Char, B)
  ; otherwise ->
    tokens(Opts, token, Tokens, A, LTS0)
  ).

%% token
tokens(Opts, token, [Token|Tokens], A, LTS) :-
  ( % character_code_constant
    A = ['0'|B],
    single_quote_char(PT_Single_Quote_Char, B, C) ->
    tokens(Opts, character_code_constant(PT,Tag,A), Tokens, PT_Single_Quote_Char, C)
  ; % binary_constant
    A = ['0', 'b'|B],
    binary_digit_char(PT_Binary_Digit_Char, B, C) ->
    tokens(Opts, binary_constant(PT,Tag,A), Tokens, PT_Binary_Digit_Char, C)
  ; % octal_constant
    A = ['0', 'o'|B],
    octal_digit_char(PT_Octal_Digit_Char, B, C) ->
    tokens(Opts, octal_constant(PT,Tag,A), Tokens, PT_Octal_Digit_Char, C)
  ; % hexadecimal_constant
    A = ['0', 'x'|B],
    hexadecimal_digit_char(PT_Hexadecimal_Char, B, C) ->
    tokens(Opts, hexadecimal_constant(PT,Tag,A), Tokens, PT_Hexadecimal_Char, C)
  ; % some number
    decimal_digit_char(PT_Decimal_Digit_Char, A, B),
    tokens(Opts, number_token(PT,Tag,A), Tokens, [PT_Decimal_Digit_Char], B)
  ; % name token
    small_letter_char(PT_Small_Letter_Char, A, B) ->
    tokens(Opts, name_token(PT,A), Tokens, PT_Small_Letter_Char, B),
    Tag = name
  ; % named variable starting with capital letter
    capital_letter_char(PT_Capital_Letter_Char, A, B) ->
    option(var_prefix(Var_Prefix), Opts),
    ( no(Var_Prefix) ->
      tokens(Opts, capital_variable(PT,A), Tokens, PT_Capital_Letter_Char, B),
      Tag = variable
    ; yes(Var_Prefix) ->
      tokens(Opts, name_token(PT,A), Tokens, PT_Capital_Letter_Char, B),
      Tag = name
    )
  ; % anonymous or named variable
    variable_indicator_char(PT_Variable_Indicator_Char, A, B) ->
    tokens(Opts, underscore_variable(PT,A), Tokens, PT_Variable_Indicator_Char, B),
    Tag = variable
  ; % comma token
    comma_char(PT_Comma_Char, A, B) ->
    PT = comma_token(PT_Comma_Char),
    Tag = comma,
    tokens(Opts, start, Tokens, B, [])
  ; % head tail separator token
    head_tail_separator_char(PT_Ht_Sep_Char, A, B) ->
    PT = head_tail_separator_token(PT_Ht_Sep_Char),
    Tag = ht_sep,
    tokens(Opts, start, Tokens, B, [])
  ; % open list token
    open_list_char(PT_Open_List_Char, A, B) ->
    PT = open_list_token(PT_Open_List_Char),
    Tag = open_list,
    tokens(Opts, start, Tokens, B, [])
  ; % close list token
    close_list_char(PT_Close_List_Char, A, B) ->
    PT = close_list_token(PT_Close_List_Char),
    Tag = close_list,
    tokens(Opts, start, Tokens, B, [])
  ; % open curly token
    open_curly_char(PT_Open_Curly_Char, A, B) ->
    PT = open_curly_token(PT_Open_Curly_Char),
    Tag = open_curly,
    tokens(Opts, start, Tokens, B, [])
  ; % close curly token
    close_curly_char(PT_Close_Curly_Char, A, B) ->
    PT = close_curly_token(PT_Close_Curly_Char),
    Tag = close_curly,
    tokens(Opts, start, Tokens, B, [])
  ; % double quoted list token
    double_quote_char(PT_Double_Quote_Char, A, B) ->
    tokens(Opts, double_quoted_list(PT,B), Tokens, PT_Double_Quote_Char, B),
    Tag = double_quoted_list
  ; % quoted_token
    single_quote_char(PT_Single_Quote_Char, A, B) ->
    tokens(Opts, quoted_token(PT,A), Tokens, PT_Single_Quote_Char, B),
    Tag = name
  ; % back quoted string token
    back_quote_char(PT_Back_Quote_Char, A, B),
    option(back_quoted_text(Back_Quoted_Text), Opts),
    yes(Back_Quoted_Text) ->
    tokens(Opts, back_quoted_string(PT,B), Tokens, PT_Back_Quote_Char, B),
    Tag = back_quoted_string
  ; % semicolon token
    semicolon_char(PT_Semicolon_Char, A, B) ->
    PT = name_token(';', semicolon_token(PT_Semicolon_Char)),
    Tag = name,
    tokens(Opts, start, Tokens, B, [])
  ; % cut token
    cut_char(PT_Cut_Char, A, B) ->
    PT = name_token('!', cut_token(PT_Cut_Char)),
    Tag = name,
    tokens(Opts, start, Tokens, B, [])
  ; % graphic token
    graphic_token_char(PT_Graphic_Token_Char, A, B) ->
    tokens(Opts, graphic_token(PT,A), Tokens, PT_Graphic_Token_Char, B),
    Tag = name
  ; % open or open_ct
    open_char(PT_Open_Char, A, B) ->
    PT = open_token(PT_Open_Char),
    ( LTS = [] ->
      Tag = open_ct
    ; otherwise ->
      Tag = open
    ),
    tokens(Opts, start, Tokens, B, [])
  ; % close token
    close_char(PT_Close_Char, A, B) ->
    PT = close_token(PT_Close_Char),
    Tag = close,
    tokens(Opts, start, Tokens, B, [])
  ),
  ( Tag = open_ct ->
    Token =.. [Tag, PT]
  ; LTS = [] ->
    Token =.. [Tag, [PT]]
  ; otherwise ->
    Token =.. [Tag, [layout_text_sequence(LTS), PT]]
  ).

%% character_code_constant/3
tokens(Opts, character_code_constant(PT,Tag,Beg), Tokens, PT_Single_Quote_Char, A) :-
  single_quoted_character(PT_Single_Quoted_Character, A, B),
  PT = integer_token(Atom, character_code_constant([
    '0',
    PT_Single_Quote_Char,
    PT_Single_Quoted_Character
  ])),
  Tag = integer,
  append(Cons, B, Beg),
  atom_chars(Atom, Cons),
  tokens(Opts, start, Tokens, B, []).

%% binary_constant/3
tokens(Opts, binary_constant(PT,Tag,Beg), Tokens, PT_Binary_Digit_Char, A) :-
  PT = integer_token(Atom, binary_constant([
    binary_constant_indicator(['0', 'b']),
    PT_Binary_Digit_Char|
    Ls
  ])),
  tokens(Opts, seq_binary_digit_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons),
  Tag = integer.

%% octal_constant/3
tokens(Opts, octal_constant(PT,Tag,Beg), Tokens, PT_Octal_Digit_Char, A) :-
  PT = integer_token(Atom, octal_constant([
    octal_constant_indicator(['0', 'o']),
    PT_Octal_Digit_Char|
    Ls
  ])),
  tokens(Opts, seq_octal_digit_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons),
  Tag = integer.

%% hexadecimal_constant/3
tokens(Opts, hexadecimal_constant(PT,Tag,Beg), Tokens, PT_Hexadecimal_Char, A) :-
  PT = integer_token(Atom, hexadecimal_constant([
    hexadecimal_constant_indicator(['0', 'x']),
    PT_Hexadecimal_Char|
    Ls
  ])),
  tokens(Opts, seq_hexadecimal_digit_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons),
  Tag = integer.

%% number_token/3
tokens(Opts, number_token(PT,Tag,Beg), Tokens, Ls0, A) :-
  ( decimal_digit_char(PT_Decimal_Digit_Char, A, B) ->
    append(Ls0, [PT_Decimal_Digit_Char], Ls1),
    tokens(Opts, number_token(PT,Tag,Beg), Tokens, Ls1, B)
  ; decimal_point_char(PT_Decimal_Point_Char, A, B),
    decimal_digit_char(PT_Decimal_Digit_Char, B, C) ->
    PT = float_number_token(Atom, [integer_constant(Ls0), fraction([PT_Decimal_Point_Char, PT_Decimal_Digit_Char|Ls])|Exponent]),
    Tag = float_number,
    tokens(Opts, fraction(Ls,Exponent,Beg,Cons), Tokens, C),
    atom_chars(Atom, Cons)
  ; otherwise ->
    Tag = integer,
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    PT = integer_token(Atom, integer_constant(Ls0)),
    tokens(Opts, start, Tokens, A, [])
  ).

%% fraction/4
tokens(_Opts, fraction([],[],Beg,Beg), [], []) :-
  !.
tokens(Opts, fraction(Ls,Exponent,Beg,Cons), Tokens, A) :-
  ( decimal_digit_char(PT_Decimal_Digit_Char, A, B) ->
    Ls = [PT_Decimal_Digit_Char|PTs],
    tokens(Opts, fraction(PTs,Exponent,Beg,Cons), Tokens, B)
  ; exponent_char(PT_Exponent_Char, A, B),
    decimal_digit_char(PT_Decimal_Digit_Char, B, C) ->
    Sign = sign([]),
    Ls = [],
    Exponent = [exponent([PT_Exponent_Char,Sign,integer_constant([PT_Decimal_Digit_Char|Rs])])],
    tokens(Opts, seq_decimal_digit_char(Rs,Beg,Cons), Tokens, C)
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = [],
    Exponent = []
  ).

%% double_quoted_list/2
tokens(Opts, double_quoted_list(PT,Beg), Tokens, PT_Double_Quote_Char, A) :-
  PT = double_quoted_list_token(Atom, [PT_Double_Quote_Char|Ls]),
  tokens(Opts, seq_double_quoted_item(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% quoted_token/2
tokens(Opts, quoted_token(PT,Beg), Tokens, PT_Single_Quote_Char, A) :-
  PT = name_token(Atom, quoted_token([PT_Single_Quote_Char|Ls])),
  tokens(Opts, seq_single_quoted_item(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% back_quoted_string/2
tokens(Opts, back_quoted_string(PT,Beg), Tokens, PT_Back_Quote_Char, A) :-
  PT = back_quoted_string_token(Atom, [PT_Back_Quote_Char|Ls]),
  tokens(Opts, seq_back_quoted_item(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% name_token/2
tokens(Opts, name_token(PT,Beg), Tokens, PT_Small_Letter_Char, A) :-
  PT = name_token(Atom, letter_digit_token([PT_Small_Letter_Char|Ls])),
  tokens(Opts, seq_alphanumeric_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% capital_variable/2
tokens(Opts, capital_variable(PT,Beg), Tokens, PT_Capital_Letter_Char, A) :-
  PT = variable_token(Atom, named_variable([PT_Capital_Letter_Char|Ls])),
  tokens(Opts, seq_alphanumeric_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% underscore_variable/2
tokens(Opts, underscore_variable(PT,Beg), Tokens, PT_Variable_Indicator_Char, A) :-
  tokens(Opts, seq_alphanumeric_char(Ls,Beg,Cons), Tokens, A),
  ( Ls = [] ->
    PT = variable_token('_', anonymous_variable(PT_Variable_Indicator_Char)),
    Beg = _ % does not matter, would only return '_'
  ; otherwise ->
    PT = variable_token(Atom, named_variable([PT_Variable_Indicator_Char|Ls])),
    atom_chars(Atom, Cons)
  ).

%% graphic_token/2
tokens(Opts, graphic_token(PT,Beg), Tokens, PT_Graphic_Token_Char, A) :-
  PT = name_token(Atom, graphic_token([PT_Graphic_Token_Char|Ls])),
  tokens(Opts, seq_graphic_token_char(Ls,Beg,Cons), Tokens, A),
  atom_chars(Atom, Cons).

%% bracketed_comment/3
tokens(Opts, bracketed_comment(LTS0,CT,Beg), Tokens, PT_Comment_Open, ['*','/'|A]) :-
  !,
  append(Cons, ['*','/'|A], Beg),
  atom_chars(Atom, Cons),
  PT = layout_text(comment(bracketed_comment([
    PT_Comment_Open,
    comment_text(Atom, CT),
    comment_close([
      comment_2_char('*'),
      comment_1_char('/')
    ])
  ]))),
  append(LTS0, [PT], LTS1),
  tokens(Opts, start, Tokens, A, LTS1).

tokens(Opts, bracketed_comment(LTS,CT0,Beg), Tokens, PT_Comment_Open, A) :-
  char(Opts, PT_Char, A, B),
  append(CT0, [PT_Char], CT1),
  tokens(Opts, bracketed_comment(LTS,CT1,Beg), Tokens, PT_Comment_Open, B).

%% single_line_comment/3
tokens(Opts, single_line_comment(LTS0,CT0,Beg), Tokens, PT_End_Line_Comment_Char, A) :-
  ( A = [] ->
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    PT = layout_text(comment(single_line_comment([
      PT_End_Line_Comment_Char,
      comment_text(Atom, CT0),
      end_of_file
    ]))),
    append(LTS0, [PT], LTS1),
    tokens(Opts, start, Tokens, [], LTS1)
  ; new_line_char(PT_New_Line_Char, A, B) ->
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    PT = layout_text(comment(single_line_comment([
      PT_End_Line_Comment_Char,
      comment_text(Atom, CT0),
      PT_New_Line_Char
    ]))),
    append(LTS0, [PT], LTS1),
    tokens(Opts, start, Tokens, B, LTS1)
  ; char(Opts, PT_Char, A, B) ->
    append(CT0, [PT_Char], CT1),
    tokens(Opts, single_line_comment(LTS0,CT1,Beg), Tokens, PT_End_Line_Comment_Char, B)
  ).

%% seq_alphanumeric_char/3
tokens(_Opts, seq_alphanumeric_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_alphanumeric_char(Ls,Beg,Cons), Tokens, A) :-
  ( alphanumeric_char(PT_Alphanumeric_Char, A, B) ->
    tokens(Opts, seq_alphanumeric_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Alphanumeric_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).

%% seq_graphic_token_char/3
tokens(_Opts, seq_graphic_token_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_graphic_token_char(Ls,Beg,Cons), Tokens, A) :-
  ( graphic_token_char(PT_Graphic_Token_Char, A, B) ->
    tokens(Opts, seq_graphic_token_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Graphic_Token_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).

%% seq_decimal_digit_char/3
tokens(_Opts, seq_decimal_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_decimal_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( decimal_digit_char(PT_Decimal_Digit_Char, A, B) ->
    tokens(Opts, seq_decimal_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Decimal_Digit_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).

%% seq_double_quoted_item/3
tokens(Opts, seq_double_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( double_quoted_item(PT_Double_Quoted_Item, A, B) ->
    tokens(Opts, seq_double_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Double_Quoted_Item|PTs]
  ; double_quote_char(PT_Double_Quote_Char, A, B) ->
    append(Cons, A, Beg),
    tokens(Opts, start, Tokens, B, []),
    Ls = [PT_Double_Quote_Char]
  ).

%% seq_back_quoted_item/3
tokens(Opts, seq_back_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( back_quoted_item(PT_Back_Quoted_Item, A, B) ->
    tokens(Opts, seq_back_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Back_Quoted_Item|PTs]
  ; back_quote_char(PT_Back_Quote_Char, A, B) ->
    append(Cons, A, Beg),
    tokens(Opts, start, Tokens, B, []),
    Ls = [PT_Back_Quote_Char]
  ).

%% seq_single_quoted_item/3
tokens(Opts, seq_single_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( single_quoted_item(PT_Single_Quoted_Item, A, B) ->
    tokens(Opts, seq_single_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Single_Quoted_Item|PTs]
  ; single_quote_char(PT_Single_Quote_Char, A, B) ->
    append(Cons, B, Beg),
    tokens(Opts, start, Tokens, B, []),
    Ls = [PT_Single_Quote_Char]
  ).

%% seq_binary_digit_char/3
tokens(_Opts, seq_binary_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_binary_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( binary_digit_char(PT_Binary_Digit_Char, A, B) ->
    tokens(Opts, seq_binary_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Binary_Digit_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).

%% seq_octal_digit_char/3
tokens(_Opts, seq_octal_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_octal_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( octal_digit_char(PT_Octal_Digit_Char, A, B) ->
    tokens(Opts, seq_octal_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Octal_Digit_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).

%% seq_hexadecimal_digit_char/3
tokens(_Opts, seq_hexadecimal_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_hexadecimal_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, A, B) ->
    tokens(Opts, seq_hexadecimal_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Hexadecimal_Digit_Char|PTs]
  ; otherwise ->
    append(Cons,A,Beg),
    tokens(Opts, start, Tokens, A, []),
    Ls = []
  ).


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
