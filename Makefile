# The MIT License

# Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

PROJECT = leptus
CT_SUITES = leptus_router leptus_req leptus_http leptus_pt leptus_config

dep_cowboy = https://github.com/extend/cowboy/archive/0.9.0.tar.gz
dep_msgpack = https://github.com/msgpack/msgpack-erlang/archive/0.2.8.tar.gz
ifdef USE_JSX
    DEPS += jsx
    dep_jsx = https://github.com/talentdeficit/jsx/archive/v1.4.5.tar.gz
    ERLC_OPTS += -DUSE_JSX
else
    DEPS += jiffy
    dep_jiffy = https://github.com/davisp/jiffy/archive/0.8.5.tar.gz
endif

EBIN = $(CURDIR)/ebin
DEPS_DIR = $(CURDIR)/deps
DOCS_DIR = $(CURDIR)/docs
TEST_DIR = $(CURDIR)/test
PLT_FILE = $(CURDIR)/.$(PROJECT).plt

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard

V ?= 0

appsrc_verbose_0 = @echo "  APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo "  ERLC  " $(?F);
erlc_verbose = $(erlc_verbose_$(V))

gen_verbose_0 = @echo "  GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

CT_OPTS ?=
CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(realpath ebin) $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs \
	$(CT_OPTS)

CT_SUITES ?=

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions

.PHONY: all deps app docs shell test dialyze clean clean-docs distclean

all: deps app

# ------------------------------------------------------------------------------
# fetch and build dependencies
# ------------------------------------------------------------------------------
deps: $(DEPS_DIR) $(patsubst dep_%,deps/%/,$(filter dep_%,$(.VARIABLES)))
	$(if $(wildcard deps/*/deps/), \
	    mv -v deps/*/deps/* deps/ && rm -rf $(wildcard deps/*/deps/))
deps/%/:
	curl -L $(word 1,$(dep_$*)) -o $@.tar.gz
	mkdir -p $(DEPS_DIR)/$*
	tar xzf $@.tar.gz -C $(DEPS_DIR)/$* --strip-components=1
	@if [ -f $@/Makefile ]; \
	then echo 'make -C $@' ; \
		make -C $@ all  ; \
	else echo 'cd $@ && rebar get-deps compile' ; \
		cd $@ && rebar get-deps compile  ; fi

# ------------------------------------------------------------------------------
# build application
# ------------------------------------------------------------------------------
app: ebin/ ebin/$(PROJECT).app

ebin/: $(wildcard src/*.erl)
	@mkdir -p $(EBIN)
	$(erlc_verbose) erlc -v -o ebin $(ERLC_OPTS) $?

ebin/$(PROJECT).app:
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

# ------------------------------------------------------------------------------
# build docs
# ------------------------------------------------------------------------------
docs:
	$(MAKE) -C $(DOCS_DIR)

# ------------------------------------------------------------------------------
# run erlang shell
# ------------------------------------------------------------------------------
shell:
	erl -pa $(EBIN) $(DEPS_DIR)/*/ebin

# ------------------------------------------------------------------------------
# run tests
# ------------------------------------------------------------------------------
test: ERLC_OPTS += -DTEST=1
test: clean deps app
	$(gen_verbose) erlc -v -o test $(ERLC_OPTS) \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) ; \
	fi
	@rm -f test/*.beam

# ------------------------------------------------------------------------------
# dialyzer
# ------------------------------------------------------------------------------
dialyze: $(PLT_FILE)
	@dialyzer +S 8 --src src --plt $(PLT_FILE) --no_native $(DIALYZER_OPTS)

PLT_APPS ?=
$(CURDIR)/%.plt:
	$(gen_verbose) dialyzer +S 8 --build_plt --output_plt $@ \
		--apps erts kernel stdlib $(PLT_APPS) \
		$(patsubst %/,%,$(filter %/,$(wildcard $(DEPS_DIR)/*/)))

# ------------------------------------------------------------------------------
# clean application
# ------------------------------------------------------------------------------
clean:
	rm -rf $(EBIN) $(CURDIR)/erl_crash.dump

# ------------------------------------------------------------------------------
# clean docs
# ------------------------------------------------------------------------------
clean-docs:
	rm -rf $(DOCS_DIR)/docs/public_html $(DOCS_DIR)/docs/tmp \
		$(DOCS_DIR)/docs/elisp

# ------------------------------------------------------------------------------
# clean project
# ------------------------------------------------------------------------------
distclean: clean clean-docs
	rm -rf $(DEPS_DIR) $(CURDIR)/logs $(CURDIR)/*.beam $(CURDIR)/.leptus.plt

$(CURDIR)/%:
	mkdir -p $@
