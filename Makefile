.PHONY: all test clean server

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

SWIPL := swipl
CLI := ./cli.exe

all: install

version:
	echo $(version)

install: install.packs cli

install.packs: install.packs.dcg4pt install.packs.clitable

install.packs.dcg4pt:
	@$(SWIPL) -q -g 'pack_install(dcg4pt,[interactive(false)]),halt(0)' -t 'halt(1)'

install.packs.clitable:
	@$(SWIPL) -q -g 'pack_install(clitable,[interactive(false)]),halt(0)' -t 'halt(1)'

upgrade: upgrade.packs

upgrade.packs: upgrade.packs.dcg4pt upgrade.packs.clitable

upgrade.packs.dcg4pt:
	@$(SWIPL) -q -g 'pack_install(dcg4pt,[interactive(false),upgrade(true)]),halt(0)' -t 'halt(1)'

upgrade.packs.clitable:
	@$(SWIPL) -q -g 'pack_install(clitable,[interactive(false),upgrade(true)]),halt(0)' -t 'halt(1)'

test: cli test.cli test.parser

test.parser:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.cli:
	echo 'Var' | $(CLI) --dcg=named_variable

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)

server:
	@$(SWIPL) -s server/server.pl --quiet -g "server('0.0.0.0':8081)"
