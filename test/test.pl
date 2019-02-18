:- use_module(library(plammar)).

:- op(800, xfx, <=>).
:- op(800, xfx, !).

/* Dynamic test generation */

:- dynamic test_definition/4.
:- dynamic path/2.

term_expansion(DCGBody: PT <=> In, test_definition(pos, DCGBody, In, PT)).
term_expansion(DCGBody: In, test_definition(pos, DCGBody, In, _)).
term_expansion(DCGBody! In, test_definition(neg, DCGBody, In, _)).

term_expansion(run(parser), Tests) :-
  set_test_paths,
  path(test/parser, TestParser_Path),
  directory_files(TestParser_Path, Test_Filenames),
  findall(
    Test_Definitions,
    (
      member(Test_Filename, Test_Filenames),
      \+member(Test_Filename, [., ..]),
      file_name_extension(Identifier, pl, Test_Filename),
      get_test_definitions(Identifier, Test_Filename, Test_Definitions)
    ),
    Nested_Test_Definitions
  ),
  flatten(Nested_Test_Definitions, Test_Definitions),
  maplist(define_tap_tests, Test_Definitions, Testss),
  flatten(Testss, Tests).

set_test_paths :-
  working_directory(CWD, CWD),
  absolute_file_name('./test', Test_Path, [relative_to(CWD), file_type(directory)]),
  assert(path(test, Test_Path)),
  absolute_file_name('./test/parser', TestParser_Path, [relative_to(CWD), file_type(directory)]),
  assert(path(test/parser, TestParser_Path)).

get_test_definitions(Identifier, Test_Filename, Sub_Tests) :-
  path(test/parser, Path),
  absolute_file_name(Test_Filename, Absolute_Filename, [relative_to(Path)]),
  load_files(Absolute_Filename, [module(Identifier)]),
  findall(
    test_definition(Absolute_Filename, Type, Body, In, Result),
    retract(test_definition(Type, Body, In, Result)),
    Sub_Tests
  ).

heads(Symbol, DCGBody, In, Head1, Head2) :-
  In = [First|_], integer(First), !,
  format(atom(Head1), '~w ~w< "~s"', [DCGBody, Symbol, In]),
  format(atom(Head2), '~w ~w> "~s"', [DCGBody, Symbol, In]).
heads(Symbol, DCGBody, In, Head1, Head2) :-
  format(atom(Head1), '~w ~w< ~p', [DCGBody, Symbol, In]),
  format(atom(Head2), '~w ~w> ~p', [DCGBody, Symbol, In]).

% succeeding ("positive") test
define_tap_tests(Test_Definition, Tests) :-
  Test_Definition = test_definition(_Filename, pos, DCGBody, In, PT),
  heads('', DCGBody, In, Head1, Head2),
  % build first test: from input to parse tree
  string_chars(In, Chars),
  Test1 = (
    Head1 :-
      plammar:tree(DCGBody, Chars, PT1), !,
      PT1 = PT  % check for correct parse tree
  ),
  tap:register_test(Head1),
  % build second test: from parse tree to input
  (nonvar(PT) ->
    Test2 = (
      Head2 :-
        plammar:tree(DCGBody, In2, PT), !,
        In2 = Chars
    ),
    tap:register_test(Head2),
    Tests = [Test1, Test2]
  ; Tests = [Test1]
  ).

% failing ("negative") test
define_tap_tests(Test_Definition, Tests) :-
  Test_Definition = test_definition(_Filename, neg, DCGBody, In, PT),
  heads('!', DCGBody, In, Head1, Head2),
  % build first test: from input to parse tree
  string_chars(In, Chars),
  Test1 = (
    Head1 :-
      \+ plammar:tree(DCGBody, Chars, _PT1), !
  ),
  tap:register_test(Head1),
  % build second test: from parse tree to input
  (nonvar(PT) ->
    Test2 = (
      Head2 :-
        \+ plammar:tree(DCGBody, _In2, PT), !
    ),
    tap:register_test(Head2),
    Tests = [Test1, Test2]
  ; Tests = [Test1]
  ).

remove(_, [], []).
remove(Elem, [Elem|Xs], Rs) :-
  remove(Elem, Xs, Rs).
remove(Elem, [X|Xs], [X|Rs]) :-
  Elem \= X,
  remove(Elem, Xs, Rs).

replace(_, _, [], []).
replace(Elem, Subst, [Elem|Xs], Res) :-
  replace(Elem, Subst, Xs, Rs),
  append(Subst, Rs, Res).
replace(Elem, Subst, [X|Xs], [X|Rs]) :-
  Elem \= X,
  replace(Elem, Subst, Xs, Rs).

canonical_test_input(In, Canonical) :-
  string_chars(In, Chars),
  replace('\n', ['\\', 'n'], Chars, Chars_N),
  string_chars(Canonical, Chars_N).

is_failing(fail).
is_failing(false).
is_failing(no).

/* End of dynamic test generation */

% define tests below
:- use_module(library(tap)).

run(parser). % replaced via term expansion

'".." is just a single token' :-
  findall(
    Tokens,
    phrase(plammar:term(term(Tokens)), ['.','.'], []),
    Solutions
  ),
  length(Solutions, 1),
  [FirstSolution] = Solutions,
  [_SingleToken] = FirstSolution.
