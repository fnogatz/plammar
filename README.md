# plammar

A Prolog Grammar written in Prolog.

## Installation

All you need is [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

Only for development purposes the [`library(tap)`](https://github.com/mndrix/tap) is needed.

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

(tbd.)
