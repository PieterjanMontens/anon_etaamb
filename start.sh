#!/bin/sh
cd ./store
erl -make
cd ../anoner
erl -make
cd ..
erl -pa store/ebin \
	-pa anoner/ebin \
	-sname anoner@localhost \
	-smp enable \
	-config ./anoner/anoner.config \
	-eval "application:start(anoner)."
