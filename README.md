# plammar

A Prolog grammar written in Prolog, for parsing and serialising Prolog code.

## Installation

First, you need [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

We make use of the following packages:
- [`library(tap)`](https://github.com/fnogatz/tap)
- [`library(dcg4pt)`](https://github.com/fnogatz/dcg4pt)
- [`library(cli_table)`](https://github.com/fnogatz/cli_table)

All can be installed by calling the following query in SWI-Prolog:

```prolog
?- pack_install(tap), pack_install(dcg4pt), pack_install(cli_table).
```

### Development Version

To get the latest development version, clone it via git and link it to the package directory of SWI-Prolog:

```sh
git clone https://github.com/fnogatz/plammar.git
ln -s $PWD/plammar $(swipl -q -g "absolute_file_name(pack(.),D,[file_type(directory)]), write(D), halt")
```

### Pre-Compilation

It is possible to create a pre-compiled file which increases the tool's performance significantly. The command line interface is compiled using swipl's [`-c` option](https://www.swi-prolog.org/pldoc/man?section=compilation):

```sh
swipl -g main -o cli.exe -c cli.pl
```

The `.exe` suffix is chosen for compatibility with Windows systems. You can also use `make cli` to generate the pre-compiled CLI.

## Usage with SWI-Prolog

First, load the package:

```prolog
?- use_module(library(plammar)).
```

Examples:

```
?- prolog_tokens(string("a(1)."), Tokens).
?- prolog_parsetree(string("a(1)."), PT).
?- prolog_ast(string("a(1)."), AST).
```

All three predicates can also take an additional `Options` list. The first argument accepts several data formats, including `string(_)`, `file(_)`, `stream(_)`, `chars(_)` and `tokens(_)`. The predicates can be used to parse and serialise Prolog source code.

For more examples, have a look at the `/test` directory.

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
