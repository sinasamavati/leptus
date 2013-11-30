PROJECT = leptus
CT_SUITES = leptus_router leptus_req leptus_http leptus_pt

.PHONY: all deps compile shell

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

shell: all
	erl -pa ebin deps/*/ebin


# github.com/extend/erlang.mk
# Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Project.

PROJECT ?= $(notdir $(CURDIR))

# Verbosity and tweaks.

V ?= 0

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter %.erl %.core,$(?F));
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

.PHONY: app clean clean-all clean-docs docs build-tests test build-plt dialyze

# Deps directory.

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))
ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

# Application.

ERL_LIBS ?= $(DEPS_DIR)
export ERL_LIBS

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard # +bin_opt_info +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))

clean-all: clean clean-docs
	$(gen_verbose) rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

app: ebin/$(PROJECT).app
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

define compile_erl
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ \
		-pa ebin/ -I include/ $(COMPILE_FIRST_PATHS) $(1)
endef

define compile_xyrl
	$(xyrl_verbose) erlc -v -o ebin/ $(1)
	$(xyrl_verbose) erlc $(ERLC_OPTS) -o ebin/ ebin/*.erl
	@rm ebin/*.erl
endef

define compile_dtl
	$(dtl_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/erlydtl/ebin/ -eval ' \
		Compile = fun(F) -> \
			Module = list_to_atom( \
				string:to_lower(filename:basename(F, ".dtl")) ++ "_dtl"), \
			erlydtl_compiler:compile(F, Module, [{out_dir, "ebin/"}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()'
endef

ebin/$(PROJECT).app: $(shell find src -type f -name \*.erl) \
		$(shell find src -type f -name \*.core) \
		$(shell find src -type f -name \*.xrl) \
		$(shell find src -type f -name \*.yrl) \
		$(shell find templates -type f -name \*.dtl 2>/dev/null)
	@mkdir -p ebin/
	$(if $(strip $(filter %.erl %.core,$?)), \
		$(call compile_erl,$(filter %.erl %.core,$?)))
	$(if $(strip $(filter %.xrl %.yrl,$?)), \
		$(call compile_xyrl,$(filter %.xrl %.yrl,$?)))
	$(if $(strip $(filter %.dtl,$?)), \
		$(call compile_dtl,$(filter %.dtl,$?)))

clean:
	$(gen_verbose) rm -rf ebin/ test/*.beam erl_crash.dump

# Documentation.

docs: clean-docs
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Tests.

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

build-test-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

build-tests: build-test-deps
	$(gen_verbose) erlc -v $(ERLC_OPTS) -o test/ \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin/

CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(realpath ebin) $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs
#	-cover test/cover.spec

CT_SUITES ?=

define test_target
test_$(1): ERLC_OPTS += -DTEST=1 +'{parse_transform, eunit_autoexport}'
test_$(1): clean deps app build-tests
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(1)) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
endef

$(foreach test,$(CT_SUITES),$(eval $(call test_target,$(test))))

test: ERLC_OPTS += -DTEST=1 +'{parse_transform, eunit_autoexport}'
test: clean app build-tests
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

# Dialyzer.

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
	-Wunmatched_returns # -Wunderspecs

build-plt: deps app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib $(PLT_APPS) $(ALL_DEPS_DIRS)

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native $(DIALYZER_OPTS)
