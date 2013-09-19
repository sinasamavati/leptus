CT_SUITES = leptus_router

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

ERLFLAGS= -pa $(CURDIR)/ebin $(CURDIR)/deps/*/ebin

.PHONY: all deps compile compile_app doc clean test shell distclean clean-ct-data

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

doc:
	./rebar skip_deps=true doc

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

compile_app:
	./rebar skip_deps=true compile

clean-ct-data:
	- rm -rf $(CURDIR)/test/*_SUITE_data/*.beam

build-tests:
	erlc -o test/ \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin/

ct: clean-ct-data build-tests
	mkdir -p logs/
	ct_run \
	-no_auto_compile -no_shell  \
	$(ERLFLAGS) \
	-dir test \
	-logdir logs \
	-suite $(addsuffix _SUITE,$(CT_SUITES))

shell: deps compile
	erl $(ERLFLAGS)

test: compile_app ct
