.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

SWIPL := swipl
CLI := ./cli.exe

all: install

version:
	echo $(version)

install: install.packs cli

install.packs: install.packs.edcgs install.packs.clitable

install.packs.edcgs:
	@$(SWIPL) -q -g 'pack_install(edcgs,[interactive(false)]),halt(0)' -t 'halt(1)'

install.packs.clitable:
	@$(SWIPL) -q -g 'pack_install(clitable,[interactive(false)]),halt(0)' -t 'halt(1)'

upgrade: upgrade.packs

upgrade.packs: upgrade.packs.edcgs upgrade.packs.clitable

upgrade.packs.edcgs:
	@$(SWIPL) -q -g 'pack_install(edcgs,[interactive(false),upgrade(true)]),halt(0)' -t 'halt(1)'

upgrade.packs.clitable:
	@$(SWIPL) -q -g 'pack_install(clitable,[interactive(false),upgrade(true)]),halt(0)' -t 'halt(1)'

test: cli test.cli test.parser

test.parser:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.cli:
	echo 'Var' | $(CLI) --dcg=named_variable

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)
