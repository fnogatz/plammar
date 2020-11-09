:- module(plammar, [
    tree/3,
    tree/4,
    prolog_tokens/2,
    prolog_tokens/3,
    prolog_parsetree/2,
    prolog_parsetree/3,
    prolog_ast/2,
    prolog_ast/3
  ]).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(readutil), [read_file_to_codes/3]).
:- use_module(library(option), [merge_options/3,option/2,option/3]).
:- use_module(library(clpfd)).

:- use_module(library(dcg4pt)).

:- use_module(plammar/environments).
:- use_module(plammar/util).
:- use_module(plammar/options).
:- use_module(plammar/pt_ast).
:- use_module(plammar/state).

prolog_tokens(A, B) :-
  prolog_tokens(A, B, []).

prolog_tokens(string(String), Tokens, Options) :-
  !,
  I0 = string_chars(String, Chars),
  I1 = prolog_tokens(chars(Chars), Tokens, Options),
  ( nonvar(String) -> Instructions = (I0, I1)
  ; Instructions = (I1, I0) ),
  Instructions.

prolog_tokens(file(File), Tokens, Options) :-
  nonvar(File),
  !,
  setup_call_cleanup(
    open(File, read, Stream),
    prolog_tokens(stream(Stream), Tokens, Options),
    close(Stream)
  ).

prolog_tokens(stream(Stream), Tokens, Options) :-
  nonvar(Stream),
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
      nonvar(Selector),
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
  nonvar(String),
  !,
  string_chars(String, Chars),
  prolog_parsetree(chars(Chars), PT, Options).
prolog_parsetree(string(String), PT, Options) :-
  nonvar(PT),
  !,
  prolog_parsetree(chars(Chars), PT, Options),
  string_chars(String, Chars).

prolog_parsetree(file(File), PT, Options) :-
  nonvar(File),
  !,
  setup_call_cleanup(
    open(File, read, Stream),
    prolog_parsetree(stream(Stream), PT, Options),
    close(Stream)
  ).

prolog_parsetree(stream(Stream), PT, Options) :-
  nonvar(Stream),
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
      nonvar(Selector),
      Selector =.. [Type, Argument]
    ),
    Types
  ),
  warning('Use one of input formats ~w', [Types]).

prolog_parsetree_(chars(Chars), PT, Options) :-
  I0 = prolog_tokens(chars(Chars), Tokens, Options),
  I1 = prolog(Options, PT, Tokens),
  ( nonvar(Chars) -> Instructions = (I0, !, I1)
  ; Instructions = (I1, !, I0) ),
  Instructions.


prolog_ast(Source, AST) :-
  prolog_ast(Source, AST, []).

prolog_ast(Source, AST, Opts0) :-
  normalise_options(prolog_parsetree, Opts0, Opts),
  I0 = prolog_parsetree(Source, PT, Opts),
  I1 = parsetree_ast(PT, AST, Opts),
  ( ground(Source) ->
    Instructions = (I0, I1)
  ; Instructions = (I1, I0) ),
  Instructions, !.

prolog_ast(Source, AST, Options) :-
  nonvar(AST),
  parsetree_ast(PT, AST, Options),
  prolog_parsetree(Source, PT, Options).

parsetree_ast(PT, AST) :-
  parsetree_ast(PT, AST, []).

parsetree_ast(PT, AST, User_Options) :-
  normalise_options(User_Options, Options),
  initial_state(Options, S0),
  pt_ast(Options, S0, SN, PT, AST),
  option(end_state(SN), Options),
  !.


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
  nonvar(Tokens),
  !,
  phrase(plammar:term(Opts, term(Tokens)), A, []).

tokens(Opts, Tokens, A) :-
  var(Tokens),
  !,
  tokens(Opts, prolog, Tokens, A, nil),
  !.


%% prolog
tokens(Opts0, prolog, [shebang(['#','!',PT_Comment_Text,NLC_Tree])|Tokens], ['#','!'|A], nil) :-
  !,
  option(allow_shebang(Allow_Shebang), Opts0, no),
  yes(Allow_Shebang),
  merge_options([disallow_chars(['\n'])], Opts0, Opts),
  comment_text(Opts, PT_Comment_Text, A, B),
  ( B = [] ->
    NLC_Tree = end_of_file,
    Tokens = []
  ; otherwise ->
    new_line_char(NLC_Tree, B, C),
    tokens(Opts0, lts, Tokens, C, DL-DL)
  ).

tokens(Opts, prolog, Tokens, A, nil) :-
  tokens(Opts, lts, Tokens, A, DL-DL).


%% start
tokens(Opts, lts, Tokens, A, LTS0-L0) :-
  ( A = [] ->
    ( L0 == LTS0 ->
      Tokens = []
    ; otherwise ->
      L0 = [],
      Tokens = [layout_text_sequence(LTS0)]
    )
  ; layout_char(PT_Layout_Char, A, B) ->
    L0 = [layout_text(PT_Layout_Char)|E1],
    tokens(Opts, lts, Tokens, B, LTS0-E1)
  ; comment_open(PT_Comment_Open, A, B) ->
    tokens(Opts, bracketed_comment(LTS0-L0,DL-DL,B), Tokens, PT_Comment_Open, B)
  ; end_line_comment_char(PT_End_Line_Comment_Char, A, B) ->
    tokens(Opts, single_line_comment(LTS0-L0,DL-DL,B), Tokens, PT_End_Line_Comment_Char, B)
  ; otherwise ->
    L0 = [],
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
    small_letter_char(Opts, PT_Small_Letter_Char, A, B) ->
    tokens(Opts, name_token(PT,A), Tokens, PT_Small_Letter_Char, B),
    Tag = name
  ; % named variable starting with capital letter
    capital_letter_char(Opts, PT_Capital_Letter_Char, A, B) ->
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
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % head tail separator token
    head_tail_separator_char(PT_Ht_Sep_Char, A, B) ->
    PT = head_tail_separator_token(PT_Ht_Sep_Char),
    Tag = ht_sep,
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % open list token
    open_list_char(PT_Open_List_Char, A, B) ->
    PT = open_list_token(PT_Open_List_Char),
    Tag = open_list,
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % close list token
    close_list_char(PT_Close_List_Char, A, B) ->
    PT = close_list_token(PT_Close_List_Char),
    Tag = close_list,
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % open curly token
    open_curly_char(PT_Open_Curly_Char, A, B) ->
    PT = open_curly_token(PT_Open_Curly_Char),
    Tag = open_curly,
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % close curly token
    close_curly_char(PT_Close_Curly_Char, A, B) ->
    PT = close_curly_token(PT_Close_Curly_Char),
    Tag = close_curly,
    tokens(Opts, lts, Tokens, B, DL-DL)
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
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % cut token
    cut_char(PT_Cut_Char, A, B) ->
    PT = name_token('!', cut_token(PT_Cut_Char)),
    Tag = name,
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % graphic token
    graphic_token_char(Opts, PT_Graphic_Token_Char, A, B) ->
    tokens(Opts, graphic_token(PT_Graphic_Token,A), Tokens, PT_Graphic_Token_Char, B),
    %% Sec. 6.4.2:
    %%   A graphic token shall not be the single character . (dot)
    %%     when . is followed by a layout char or single line comment.
    ( PT_Graphic_Token = name_token('.', _),
      ( layout_char(_, B, _) ; B = ['%'|_] ; B = [] ) ->
      Tag = end,
      PT = end_token(end_char('.'))
    ; otherwise ->
      Tag = name,
      PT = PT_Graphic_Token
    )
  ; % open or open_ct
    open_char(PT_Open_Char, A, B) ->
    PT = open_token(PT_Open_Char),
    ( LTS = [] ->
      Tag = open_ct
    ; otherwise ->
      Tag = open
    ),
    tokens(Opts, lts, Tokens, B, DL-DL)
  ; % close token
    close_char(PT_Close_Char, A, B) ->
    PT = close_token(PT_Close_Char),
    Tag = close,
    tokens(Opts, lts, Tokens, B, DL-DL)
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
  ( single_quoted_character(Opts, PT_Single_Quoted_Character, A, B)
  ; option(allow_single_quote_char_in_character_code_constant(Allow_Single_Quote_Char_In_Character_Code_Constant), Opts, no),
    yes(Allow_Single_Quote_Char_In_Character_Code_Constant),
    A = ['\''|B],
    PT_Single_Quoted_Character = single_quoted_character(single_quote_char('\''))
  ),
  PT = integer_token(Atom, character_code_constant([
    '0',
    PT_Single_Quote_Char,
    PT_Single_Quoted_Character
  ])),
  Tag = integer,
  append(Cons, B, Beg),
  atom_chars(Atom, Cons),
  tokens(Opts, lts, Tokens, B, DL-DL).

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
  ; underscore_char(PT_Underscore_Char, A, B),
    option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
    yes(Allow_Digit_Groups_With_Underscore) ->
    ( decimal_digit_char(PT_Decimal_Digit_Char, B, D) ->
      append(Ls0, [PT_Underscore_Char, PT_Decimal_Digit_Char], Ls1)
    ; bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
      decimal_digit_char(PT_Decimal_Digit_Char, C, D) ->
      append(Ls0, [PT_Underscore_Char, PT_Bracketed_Comment, PT_Decimal_Digit_Char], Ls1)
    ),
    tokens(Opts, number_token(PT,Tag,Beg), Tokens, Ls1, D)
  ; space_char(PT_Space_Char, A, B),
    option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
    yes(Allow_Digit_Groups_With_Space),
    decimal_digit_char(PT_Decimal_Digit_Char, B, C) ->
    append(Ls0, [PT_Space_Char, PT_Decimal_Digit_Char], Ls1),
    tokens(Opts, number_token(PT,Tag,Beg), Tokens, Ls1, C)
  ; decimal_point_char(PT_Decimal_Point_Char, A, B),
    decimal_digit_char(PT_Decimal_Digit_Char, B, C) ->
    PT = float_number_token(Atom, [integer_constant(Ls0), fraction([PT_Decimal_Point_Char, PT_Decimal_Digit_Char|Ls])|Exponent]),
    Tag = float_number,
    tokens(Opts, fraction(Ls,Exponent,Beg,Cons), Tokens, C),
    atom_chars(Atom, Cons)
  ; exponent_char(PT_Exponent_Char, A, B),
    option(allow_integer_exponential_notation(Allow_Integer_Exponential_Notation), Opts, no),
    yes(Allow_Integer_Exponential_Notation),
    sign(PT_Sign, B, C),
    decimal_digit_char(PT_Decimal_Digit_Char, C, D) ->
    PT = float_number_token(Atom, [integer_constant(Ls0)|Exponent]),
    Tag = float_number,
    Exponent = [exponent([PT_Exponent_Char,PT_Sign,integer_constant([PT_Decimal_Digit_Char|Rs])])],
    tokens(Opts, seq_decimal_digit_char(Rs,Beg,Cons), Tokens, D),
    atom_chars(Atom, Cons)
  ; otherwise ->
    Tag = integer,
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    PT = integer_token(Atom, integer_constant(Ls0)),
    tokens(Opts, lts, Tokens, A, DL-DL)
  ).

%% fraction/4
tokens(_Opts, fraction([],[],Beg,Beg), [], []) :-
  !.
tokens(Opts, fraction(Ls,Exponent,Beg,Cons), Tokens, A) :-
  ( decimal_digit_char(PT_Decimal_Digit_Char, A, B) ->
    Ls = [PT_Decimal_Digit_Char|PTs],
    tokens(Opts, fraction(PTs,Exponent,Beg,Cons), Tokens, B)
  ; exponent_char(PT_Exponent_Char, A, B),
    sign(PT_Sign, B, C),
    decimal_digit_char(PT_Decimal_Digit_Char, C, D) ->
    Ls = [],
    Exponent = [exponent([PT_Exponent_Char,PT_Sign,integer_constant([PT_Decimal_Digit_Char|Rs])])],
    tokens(Opts, seq_decimal_digit_char(Rs,Beg,Cons), Tokens, D)
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
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
tokens(Opts, bracketed_comment(LTS0-L0,CT-[],Beg), Tokens, PT_Comment_Open, ['*','/'|A]) :-
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
  L0 = [PT|L1],
  tokens(Opts, lts, Tokens, A, LTS0-L1).

tokens(Opts, bracketed_comment(LTS0-L0,CT0-E0,Beg), Tokens, PT_Comment_Open, A) :-
  char(Opts, PT_Char, A, B),
  E0 = [PT_Char|E1],
  tokens(Opts, bracketed_comment(LTS0-L0,CT0-E1,Beg), Tokens, PT_Comment_Open, B).

%% single_line_comment/3
tokens(Opts, single_line_comment(LTS0-L0,CT0-E0,Beg), Tokens, PT_End_Line_Comment_Char, A) :-
  ( A = [] ->
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    E0 = [],
    PT = layout_text(comment(single_line_comment([
      PT_End_Line_Comment_Char,
      comment_text(Atom, CT0),
      end_of_file
    ]))),
    L0 = [PT|L1],
    tokens(Opts, lts, Tokens, [], LTS0-L1)
  ; new_line_char(PT_New_Line_Char, A, B) ->
    append(Cons, A, Beg),
    atom_chars(Atom, Cons),
    E0 = [],
    PT = layout_text(comment(single_line_comment([
      PT_End_Line_Comment_Char,
      comment_text(Atom, CT0),
      PT_New_Line_Char
    ]))),
    L0 = [PT|L1],
    tokens(Opts, lts, Tokens, B, LTS0-L1)
  ; char(Opts, PT_Char, A, B) ->
    E0 = [PT_Char|E1],
    tokens(Opts, single_line_comment(LTS0-L0,CT0-E1,Beg), Tokens, PT_End_Line_Comment_Char, B)
  ).

%% seq_alphanumeric_char/3
tokens(_Opts, seq_alphanumeric_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_alphanumeric_char(Ls,Beg,Cons), Tokens, A) :-
  ( alphanumeric_char(Opts, PT_Alphanumeric_Char, A, B) ->
    tokens(Opts, seq_alphanumeric_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Alphanumeric_Char|PTs]
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
    Ls = []
  ).

%% seq_graphic_token_char/3
tokens(_Opts, seq_graphic_token_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_graphic_token_char(Ls,Beg,Cons), Tokens, A) :-
  ( graphic_token_char(Opts, PT_Graphic_Token_Char, A, B) ->
    tokens(Opts, seq_graphic_token_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Graphic_Token_Char|PTs]
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
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
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
    Ls = []
  ).

%% seq_double_quoted_item/3
tokens(Opts, seq_double_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( double_quoted_item(Opts, PT_Double_Quoted_Item, A, B) ->
    tokens(Opts, seq_double_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Double_Quoted_Item|PTs]
  ; double_quote_char(PT_Double_Quote_Char, A, B) ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, B, DL-DL),
    Ls = [PT_Double_Quote_Char]
  ).

%% seq_back_quoted_item/3
tokens(Opts, seq_back_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( back_quoted_item(Opts, PT_Back_Quoted_Item, A, B) ->
    tokens(Opts, seq_back_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Back_Quoted_Item|PTs]
  ; back_quote_char(PT_Back_Quote_Char, A, B) ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, B, DL-DL),
    Ls = [PT_Back_Quote_Char]
  ).

%% seq_single_quoted_item/3
tokens(Opts, seq_single_quoted_item(Ls,Beg,Cons), Tokens, A) :-
  ( single_quoted_item(Opts, PT_Single_Quoted_Item, A, B) ->
    tokens(Opts, seq_single_quoted_item(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Single_Quoted_Item|PTs]
  ; single_quote_char(PT_Single_Quote_Char, A, B) ->
    append(Cons, B, Beg),
    tokens(Opts, lts, Tokens, B, DL-DL),
    Ls = [PT_Single_Quote_Char]
  ).

%% seq_binary_digit_char/3
tokens(_Opts, seq_binary_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_binary_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( binary_digit_char(PT_Binary_Digit_Char, A, B) ->
    tokens(Opts, seq_binary_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Binary_Digit_Char|PTs]
  ; underscore_char(PT_Underscore_Char, A, B),
    option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
    yes(Allow_Digit_Groups_With_Underscore) ->
    ( binary_digit_char(PT_Binary_Digit_Char, B, D) ->
      Ls = [PT_Underscore_Char, PT_Binary_Digit_Char|PTs]
    ; bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
      binary_digit_char(PT_Binary_Digit_Char, C, D) ->
      Ls = [PT_Underscore_Char, PT_Bracketed_Comment, PT_Binary_Digit_Char|PTs]
    ),
    tokens(Opts, seq_binary_digit_char(PTs,Beg,Cons), Tokens, D)
  ; space_char(PT_Space_Char, A, B),
    option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
    yes(Allow_Digit_Groups_With_Space),
    binary_digit_char(PT_Binary_Digit_Char, B, C) ->
    Ls = [PT_Space_Char, PT_Binary_Digit_Char|PTs],
    tokens(Opts, seq_binary_digit_char(PTs,Beg,Cons), Tokens, C)
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
    Ls = []
  ).

%% seq_octal_digit_char/3
tokens(_Opts, seq_octal_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_octal_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( octal_digit_char(PT_Octal_Digit_Char, A, B) ->
    tokens(Opts, seq_octal_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Octal_Digit_Char|PTs]
  ; underscore_char(PT_Underscore_Char, A, B),
    option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
    yes(Allow_Digit_Groups_With_Underscore) ->
    ( octal_digit_char(PT_Octal_Digit_Char, B, D) ->
      Ls = [PT_Underscore_Char, PT_Octal_Digit_Char|PTs]
    ; bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
      octal_digit_char(PT_Octal_Digit_Char, C, D) ->
      Ls = [PT_Underscore_Char, PT_Bracketed_Comment, PT_Octal_Digit_Char|PTs]
    ),
    tokens(Opts, seq_octal_digit_char(PTs,Beg,Cons), Tokens, D)
  ; space_char(PT_Space_Char, A, B),
    option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
    yes(Allow_Digit_Groups_With_Space),
    octal_digit_char(PT_Octal_Digit_Char, B, C) ->
    Ls = [PT_Space_Char, PT_Octal_Digit_Char|PTs],
    tokens(Opts, seq_octal_digit_char(PTs,Beg,Cons), Tokens, C)
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
    Ls = []
  ).

%% seq_hexadecimal_digit_char/3
tokens(_Opts, seq_hexadecimal_digit_char([],Beg,Beg), [], []) :-
  !.
tokens(Opts, seq_hexadecimal_digit_char(Ls,Beg,Cons), Tokens, A) :-
  ( hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, A, B) ->
    tokens(Opts, seq_hexadecimal_digit_char(PTs,Beg,Cons), Tokens, B),
    Ls = [PT_Hexadecimal_Digit_Char|PTs]
  ; underscore_char(PT_Underscore_Char, A, B),
    option(allow_digit_groups_with_underscore(Allow_Digit_Groups_With_Underscore), Opts, no),
    yes(Allow_Digit_Groups_With_Underscore) ->
    ( hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, B, D) ->
      Ls = [PT_Underscore_Char, PT_Hexadecimal_Digit_Char|PTs]
    ; bracketed_comment(Opts, PT_Bracketed_Comment, B, C),
      hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, C, D) ->
      Ls = [PT_Underscore_Char, PT_Bracketed_Comment, PT_Hexadecimal_Digit_Char|PTs]
    ),
    tokens(Opts, seq_hexadecimal_digit_char(PTs,Beg,Cons), Tokens, D)
  ; space_char(PT_Space_Char, A, B),
    option(allow_digit_groups_with_space(Allow_Digit_Groups_With_Space), Opts, no),
    yes(Allow_Digit_Groups_With_Space),
    hexadecimal_digit_char(PT_Hexadecimal_Digit_Char, B, C) ->
    Ls = [PT_Space_Char, PT_Hexadecimal_Digit_Char|PTs],
    tokens(Opts, seq_hexadecimal_digit_char(PTs,Beg,Cons), Tokens, C)
  ; otherwise ->
    append(Cons, A, Beg),
    tokens(Opts, lts, Tokens, A, DL-DL),
    Ls = []
  ).


%% "A token shall not be followed by characters such that
%%   concatenating the characters of the token with these
%%   characters forms a valid token as specified by the above
%%   Syntax." (6.4)
token(Opts, Tree, In, Rest) :-
  nonvar(In), !,
  token_(Opts, token_(Tree), In, Rest),
  Some_More_Elements = [_|_], % at least one element
  \+((
    token_(Opts, _, In, Shorter_Rest),
    append(Some_More_Elements, Shorter_Rest, Rest)
  )).
token(Opts, Tree, In, Rest) :-
  nonvar(Tree), !,
  token_(Opts, token_(Tree), In, Rest).
token(_Opts, Tree, In, Rest) :-
  var(Tree), var(In), !,
  warning('Parse tree AND input unbound; this might not work as expected!'),
  token_(token_(Tree), In, Rest).

:- op(600, xfx, token).
:- discontiguous plammar:token/4.

term_expansion(X1 token Opts --> Y1, [Rule]) :-
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

term_expansion(Head wrap_text --> Y1, [Rule]) :-
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

term_expansion(X1 --> Y1, [Rule]) :-
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
  we want to create the smallest possibilities
  at first.
*/
:- op(800, fy, *).
*(DCGBody, Tree, In, Out) :-
  % only if input list is given
  nonvar(In), !,
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
