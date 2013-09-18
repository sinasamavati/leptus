ERL ?= erl
APP := leptus

.PHONY: deps

all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile

clean:
	rebar clean

distclean: clean
	rebar delete-deps

docs:
	erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

compile_app:
	rebar compile skip_deps=true

test: compile_app
	rebar ct skip_deps=true
