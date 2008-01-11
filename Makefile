ERL=/Users/rsaccon/R11B/start.sh
#ERL=/usr/local/erlware/bin/erl
#ERL=erl
APP_NAME=erlydtl


all:
	$(ERL) -make 

run:
	$(ERL) -pa `pwd`/ebin -s $(APP_NAME)
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump