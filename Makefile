ERL=erl
ERLC=erlc
REBAR=./rebar

all: compile

compile:
	@$(REBAR) compile

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
		-s init stop

clean:
	@$(REBAR) clean
	rm -fv ebintest/*
	rm -fv erl_crash.dump
	rm -fv tests/output/*
