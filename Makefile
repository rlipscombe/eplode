PROJECT = eplode
DEPS = hackney idna
dep_idna = git https://github.com/benoitc/erlang-idna 1.0.3
dep_hackney = git https://github.com/benoitc/hackney 1.4.8

include erlang.mk

dev:: app
	erl -pa ebin -pa deps/*/ebin -s eplode
