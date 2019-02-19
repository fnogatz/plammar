:- use_module(library(plammar)).

:- op(800, xfx, <=>).
:- op(800, xfx, !).

:- load_files('predicates/prolog_tokens.pl').

/* Dynamic test generation */

:- dynamic test_definition/4.
:- dynamic path/2.

term_expansion(DCGBody: PT <=> In, test_definition(pos, DCGBody, In, PT)).
term_expansion(DCGBody: In, test_definition(pos, DCGBody, In, _)).
term_expansion(DCGBody! In, test_definition(neg, DCGBody, In, _)).

term_expansion(run(prolog_tokens/2), Tests) :-
   findall(
      eq(A,B),
      '<=>'(A,B),
      Eqs
   ),
   maplist(define_predicate_test(prolog_tokens), Eqs, Tests).

term_expansion(run(tokenizer), Tests) :-
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

term_expansion(run(prolog, Type), Tests) :-
  path(test/prolog/Type, TestProlog_Path),
  directory_files(TestProlog_Path, Test_Filenames),
  findall(
    Test_Definitions,
    (
      member(Test_Filename, Test_Filenames),
      \+member(Test_Filename, [., ..]),
      file_name_extension(Identifier, pl, Test_Filename),
      get_prolog_test_definitions(Type, Identifier, Test_Filename, Test_Definitions)
    ),
    Nested_Test_Definitions
  ),
  flatten(Nested_Test_Definitions, Tests).

define_predicate_test(Predicate, eq(A,B), Test) :-
  format(atom(Head), '~w: ~w', [Predicate, A]),
  Test = (
    Head :-
      call(Predicate, A, B1),
      B1 = B,
      !,
      call(Predicate, A1, B),
      A1 = A,
      !
  ),
  tap:register_test(Head).

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
  Test_Definition = test_definition(_Filename, pos, DCGGoal, In, PT),
  DCGGoal =.. [DCGBody|_DCGArguments],
  heads('', DCGBody, In, Head1, Head2),
  % build first test: from input to parse tree
  string_chars(In, Chars),
  Test1 = (
    Head1 :-
      plammar:tree(DCGGoal, Chars, PT1), !,
      PT1 = PT  % check for correct parse tree
  ),
  tap:register_test(Head1),
  % build second test: from parse tree to input
  (nonvar(PT) ->
    Test2 = (
      Head2 :-
        plammar:tree(DCGGoal, In2, PT), !,
        In2 = Chars
    ),
    tap:register_test(Head2),
    Tests = [Test1, Test2]
  ; Tests = [Test1]
  ).

% failing ("negative") test
define_tap_tests(Test_Definition, Tests) :-
  Test_Definition = test_definition(_Filename, neg, DCGGoal, In, PT),
  DCGGoal =.. [DCGBody|_DCGArguments],
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

prolog_parsetrees(In, PTs, Options) :-
  findall(
    PT,
    prolog_parsetree(In, PT, Options),
    PTs
  ).

/* End of dynamic test generation */

:- set_test_paths.

:- use_module(library(tap)).

% tests are defined below

run(tokenizer). % replaced via term expansion

'".." is just a single token' :-
  findall(
    Tokens,
    phrase(plammar:term(term(Tokens)), ['.','.'], []),
    Solutions
  ),
  length(Solutions, 1),
  [FirstSolution] = Solutions,
  [_SingleToken] = FirstSolution.

%% Prolog predicates

run(prolog_tokens/2). % replaced via term expansion

:- load_files('predicates/prolog_parsetree.pl').
