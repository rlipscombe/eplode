PROJECT = eplode
include erlang.mk

dev:: app
	erl -pa ebin -pa deps/*/ebin -s eplode
