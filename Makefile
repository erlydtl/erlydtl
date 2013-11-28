ERL=erl
ERLC=erlc
REBAR=./rebar $(REBAR_ARGS)

all: compile

compile: check-slex
	@$(REBAR) compile

check-slex: src/erlydtl_scanner.erl
src/erlydtl_scanner.erl: src/erlydtl_scanner.slex
	@echo Notice: $@ is outdated by $<, consider running "'make slex'".

compile_test:
	-mkdir -p ebintest
	$(ERLC) -o tests/src -I include/erlydtl_preparser.hrl tests/src/erlydtl_extension_testparser.yrl
	$(ERL) -make

test: compile compile_test
	$(ERL) -noshell -pa ebin -pa ebintest \
		-s erlydtl_functional_tests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s erlydtl_unittests run_tests \
		-s sources_parser_unittests run_tests \
		-eval 'eunit:test(erlydtl_tsd_compiler, [verbose])' \
		-s init stop

clean:
	@$(REBAR) clean
	rm -fv ebintest/*
	rm -fv erl_crash.dump
	rm -fv tests/output/*

# rebuild any .slex files as well..  not included by default to avoid
# the slex dependency, which is only needed in case the .slex file has
# been modified locally.
slex: REBAR_DEPS ?= get-deps update-deps
slex: slex-compile

slex-skip-deps: REBAR_DEPS:=
slex-skip-deps: slex-compile

slex-compile:
	@$(REBAR) -C rebar-slex.config $(REBAR_DEPS) compile
