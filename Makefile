ERL=erl

all:
	$(ERL) -make 

run:
	$(ERL) -pa `pwd`/ebin

test:
	$(ERL) -noshell -s erlydtl_unittests run_tests
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump
