PROJECT = eplode

DEPS += lager

DEPS += hackney
dep_hackney = git https://github.com/benoitc/hackney 1.6.0

DEPS += cowboy
DEPS += erlydtl

DEPS += sync

include erlang.mk

ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

dev:: app
	erl -pa ebin -pa deps/*/ebin -s eplode
