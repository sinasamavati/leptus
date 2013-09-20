PROJECT = leptus
CT_SUITES = leptus_router leptus_http

.PHONY: all deps compile shell dev

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

shell: all
	erl -pa ebin deps/*/ebin

dev: all
	./rebar -C rebar_dev.config get-deps compile

include erlang.mk
