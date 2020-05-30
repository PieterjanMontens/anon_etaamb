#!/bin/sh
cd ./store
erl -make
cd ../anoner
erl -make
cd ../server
erl -make
cd ..
erl -pa store/ebin \
	-pa anoner/ebin \
	-pa server/ebin \
	-pa server/deps/mochiweb/ebin \
	-sname anoner@localhost \
	-smp enable \
	-config ./conf.config \
	-eval "application:start(anoner)."
