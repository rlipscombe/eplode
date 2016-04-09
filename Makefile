PROJECT = eplode

DEPS += hackney
dep_hackney = git https://github.com/benoitc/hackney 1.6.0

DEPS += cowboy

DEPS += sync

include erlang.mk

dev:: app
	erl -pa ebin -pa deps/*/ebin -s eplode
