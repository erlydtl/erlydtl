ERL=/Users/rsaccon/R11B/start.sh
#ERL=/usr/local/erlware/bin/erl
#ERL=erl
APP_NAME=erlydtl


all:
	( $(ERL) -make && \
	if [ ! -e ebin/$(APP_NAME).app ]; then cp -f src/$(APP_NAME)/$(APP_NAME).app.src ebin/$(APP_NAME).app; fi )	

run:
	$(ERL) -pa `pwd`/ebin
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump