PROJECT = leptus
CT_SUITES = leptus_router

.PHONY: all deps compile shell

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

shell: all
	erl -pa ebin deps/*/ebin

include erlang.mk
