ERL=erl
ERLC=erlc

PARSER=src/erlydtl/erlydtl_parser

all: $(PARSER).erl ebin/erlydtl.app
	$(ERL) -make 

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

ebin/erlydtl.app: ebin src/erlydtl/erlydtl.app
	@cp src/erlydtl/erlydtl.app $<

ebin:
	mkdir ebin
 
run:
	$(ERL) -pa ebin


test:
	$(ERL) -noshell -pa ebin \
		-s erlydtl_functional_tests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s erlydtl_unittests run_tests \
		-s init stop
	
clean:
	rm -f ebin/*.beam ebin/erlydtl.app
	rm -f erl_crash.dump $(PARSER).erl
