ERL=/Users/rsaccon/R11B/start.sh
#ERL=/usr/local/erlware/bin/erl
#ERL=erl


all:
	$(ERL) -make

run:
	$(ERL) -pa `pwd`/ebin
	
clean:
	rm -fv ebin/*
	rm -fv erl_crash.dump