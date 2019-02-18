# TAP Input/Output Test Suite

Definition of input/output tests for `plammar` parser following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command from the project's root directory:

```shell
> swipl -q -g main -t halt -s test/test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..4
ok 1 - integer_token < "123"
ok 2 - integer_token > "123"
ok 3 - integer_token < "00123"
ok 4 - integer_token !< "12 34"
```

The identifier given first references the tested DCG Body (e.g., `integer_token`), the string the called input. `>` denotes that from a given parse tree the correct input is created, and `<` the other way around. `!>` and `!<` denote tests designed to fail, e.g., `"12 34"` is no valid integer token.

## Define Tests

Each file in the `parser` directory specifies some tests. For simple input/output tests, we provide a term expansion to specify the tests in the following form:

```prolog
DCGBody: ParseTree <=> String.
```

Tests which do not check the parse tree can be specified in a shorter form:

```prolog
DCGBody: String.
```

To define failing tests, i.e. to define inputs that should be recognized as no valid instances of the given DCG body, simply use `!` instead of `:`.
