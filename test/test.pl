:- asserta(user:file_search_path(library, prolog)).
:- use_module(library(ast)).

/* Dynamic test generation */

:- dynamic test_definition/3.
:- dynamic path/2.

term_expansion((Input: Result), test_definition(Body, In, Result)) :-
  Input =.. [Body, In].

term_expansion(run_tests, Tests) :-
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
  maplist(define_tap_test, Test_Definitions, Tests),
  maplist(get_tap_test_name, Test_Definitions, Tap_Test_Names),
  forall(
    member(Tap_Test_Name, Tap_Test_Names),
    tap:register_test(Tap_Test_Name)
  ).

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
    test_definition(Absolute_Filename, Body, In, Result),
    retract(test_definition(Body, In, Result)),
    Sub_Tests
  ).

define_tap_test(Test_Definition, Tap_Test) :-
  get_tap_test_name(Test_Definition, Tap_Test_Name),
  get_tap_test_run(Test_Definition, Test_Run),
  Test = (
    once(Test_Run),
    !
  ),
  Tap_Test = (Tap_Test_Name :- Test).

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

get_tap_test_name(test_definition(_Filename, Body, In, Result), Name) :-
  is_failing(Result),
  canonical_test_input(In, In_C),
  format(atom(Name), '[~w -] ~w', [Body, In_C]).

get_tap_test_name(test_definition(_Filename, Body, In, Result), Name) :-
  \+is_failing(Result),
  canonical_test_input(In, In_C),
  format(atom(Name), '[~w +] ~w', [Body, In_C]).

get_tap_test_run(test_definition(_Filename, Body, In, Result), Run) :-
  is_failing(Result),
  % test must fail
  Run = (
    string_chars(In, Chars),
    !,
    \+ast:tree(Body, Chars, _)
  ).

get_tap_test_run(test_definition(_Filename, Body, In, true), Run) :-
  % test must succeed regardless the actual result
  Run = (
    string_chars(In, Chars),
    ast:tree(Body, Chars, _)
  ).

get_tap_test_run(test_definition(_Filename, Body, In, Result), Run) :-
  \+is_failing(Result),
  Result \= true,
  % test must succeed with given result
  Run = (
    string_chars(In, Chars),
    ast:tree(Body, Chars, Result)
  ).

is_failing(fail).
is_failing(false).
is_failing(no).

/* End of dynamic test generation */

% define tests below
:- use_module(library(tap)).

 % replaced via term expansion
run_tests.
