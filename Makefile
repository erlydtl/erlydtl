ERL=erl
ERLC=erlc

PARSER=src/erlydtl_parser


all: compile

compile: $(PARSER).erl
	-mkdir -p ebintest
	$(ERL) -make 

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src src/erlydtl_parser.yrl
 
run: compile
	$(ERL) -pa ebin


test: compile
	$(ERL) -noshell -pa ebin -pa ebintest \
		-s erlydtl_functional_tests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s erlydtl_unittests run_tests \
		-s sources_parser_unittests run_tests \
		-s init stop
	
clean:
	rm -fv ebin/*.beam
	rm -fv ebintest/*
	rm -fv erl_crash.dump $(PARSER).erl
	rm -fv tests/output/*
