# plammar

A Prolog Grammar written in Prolog.

## Installation

First, you need [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

We make use of the following packages:
- [`library(dcg4pt)`](https://github.com/fnogatz/dcg4pt)
- [`library(clitable)`](https://github.com/fnogatz/clitable)

Both can be installed by calling `?- pack_install(dcg4pt).` resp. `?- pack_install(clitable)` in SWI-Prolog.

Only for development purposes the [`library(tap)`](https://github.com/mndrix/tap) is needed, which can be installed the same way.

### Pre-Compilation

It is possible to create a pre-compiled file which increases the tool's performance significantly. The command line interface is compiled using swipl's [`-c` option](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%272.10%27,swi%28%27/doc/Manual/compilation.html%27%29%29):

```sh
swipl -g main -o cli.exe -c cli.pl
```

The `.exe` suffix is chosen for compatibility with Windows systems. You can also use `make cli` to generate the pre-compiled CLI.

## Usage as CLI

plammar comes with a command line interface to parse given source code. You can directly execute it via

```sh
swipl -g main cli.pl -- [options] [<filename>]
```

Call with `--help` instead of the filenames to get more options. The CLI accepts a filename as the first argument. If called without this filename, the source is read from stdin.

After the pre-compilation step mentioned before, the created executable can be called via:

```sh
./cli.exe [options] [<filename>]
```

## Usage with SWI-Prolog

Load the `ast.pl` script as follows:

```sh
> swipl -s prolog/ast.pl
```

Then, you can interact with the Prolog parser, e.g.:

```prolog
?- In = "a b c",
parse(term(Prec, ops(Ops,Nots), AST), In),
writeln('-----------'),
ast:pp(AST), nl.
```

In `Ops`, the inferred operators are given. In `Nots`, all constraints on atoms which are not allowed to be an operator are specified. `Prec` is the inferred precedence of the given term with these operator definition. `AST` is the abstract syntax tree of the parsed term.

### Examples

The previous call yields for instance the following allowed operator definitions:

```prolog
Ops = [op(Prec, xfx, b)],
Nots = [op(_, _, a), op(_, _, c)],
Prec in ..(1, 1201)
----------------------------------
Ops = [op(Prec, xfy, b)],
Nots = [op(_, _, a), op(_, _, c)],
Prec in ..(1, 1201)
----------------------------------
Ops = [op(1201, xfx, b), op(_, _, a)],
Nots = [op(_, _, c)]
Prec = 1201
----------------------------------
Ops = [op(Prec, xfy, b)],
Nots = [op(_, _, a), op(_, _, c)],
Prec in ..(1, 1201)
----------------------------------
  ...
----------------------------------
Ops = [op(Prec, yf, c), op(_, yf, b)],
Nots = [op(P1, _, a)],
Prec in ..(0, 1201),
Prec #>= P1,
P1 in ..(0, 1201)
----------------------------------
  ...
```

## TODO list

- `- 1` is currently accepted
- Extend testing framework:
  - Test for valid and invalid terms of Table 5 on ISO Standard (p. 17)
