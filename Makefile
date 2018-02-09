.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

SWIPL := swipl
CLI := ./cli.exe

all: install

version:
	echo $(version)

install: cli

test: cli test.cli test.parser

test.parser:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.cli:
	echo 'Var' | $(CLI) --dcg=named_variable

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)
