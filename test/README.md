# TAP Input/Output Test Suite

Definition of input/output tests for `plammar` parser following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..4
ok 1 - [integer_token +] 123
ok 2 - [integer_token -] 1.2
ok 3 - [float_number_token +] 1.2
ok 4 - [float_number_token -] 1. 2
```

The identifier given in the square brackets references the tested DCG Body, the following identifies the called input. `+` in the square brackets denotes positive tests, `-` denotes input that should fail.

## Define Tests

Each file in the `parser` directory specifies some tests. There, new tests can be specified in the following form:

```prolog
dcg(Input):
  Output.
```

To define failing tests, i.e. to define inputs that should be recognized as no valid instances of the given DCG body, simply use `fail` as `Output`. It also possible to use `true` as `Output` if the actual return value does not matter.
