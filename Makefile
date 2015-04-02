# The MIT License
#
# Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

PROJECT = leptus
CT_SUITES = leptus_pt leptus_router leptus_handler leptus_logger leptus_http \
	leptus_config leptus_utils

dep_cowboy = https://github.com/extend/cowboy/archive/0.9.0.tar.gz
dep_msgpack = https://github.com/msgpack/msgpack-erlang/archive/0.2.8.tar.gz
ifdef USE_JSX
    dep_jsx = https://github.com/talentdeficit/jsx/archive/v1.4.5.tar.gz
    ERLC_OPTS += -DUSE_JSX
else
    dep_jiffy = https://github.com/davisp/jiffy/archive/0.8.5.tar.gz
endif

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

EBIN = $(CURDIR)/ebin
DOCS_DIR = $(CURDIR)/docs
TEST_DIR = $(CURDIR)/test
PLT_FILE = $(CURDIR)/.$(PROJECT).plt

ERLC_OPTS += -I include -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard

V ?= 0

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
.PRECIOUS: $(DEPS_DIR)/%.tar.gz

deps: $(DEPS_DIR) $(patsubst dep_%,$(DEPS_DIR)/%/,$(filter dep_%,$(.VARIABLES)))
	$(if $(shell find $(DEPS_DIR)/*/deps/* -maxdepth 1 -type d 2> /dev/null), \
	    mv -v $(DEPS_DIR)/*/deps/* $(DEPS_DIR) && rm -rf $(DEPS_DIR)/*/deps/)

$(DEPS_DIR):
	mkdir -p $(DEPS_DIR)

$(DEPS_DIR)/%/: $(DEPS_DIR)/%.tar.gz
	mkdir -p $(DEPS_DIR)/$*
	tar xzf $(DEPS_DIR)/$*.tar.gz -C $(DEPS_DIR)/$* --strip-components=1
	@if [ -f $(DEPS_DIR)/$*/Makefile ]; \
	then $(MAKE) -C $(DEPS_DIR)/$* all ; \
	else echo 'cd $@ && rebar get-deps compile' ; \
		cd $(DEPS_DIR)/$* && rebar get-deps compile ; fi

$(DEPS_DIR)/%.tar.gz:
	curl -L $(word 1,$(dep_$*)) -o $(DEPS_DIR)/$*.tar.gz

# ------------------------------------------------------------------------------
# build application
# ------------------------------------------------------------------------------
app: ebin/ ebin/$(PROJECT).app

ebin/: $(wildcard src/*.erl)
	@mkdir -p $(EBIN)
	$(erlc_verbose) erlc -v -o ebin $(ERLC_OPTS) $?

ebin/%.app: src/%.app.src
	cp $< $@

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
test: ERLC_OPTS += -DTEST=1 +export_all
test: clean deps app
	$(gen_verbose) erlc -v -o test $(ERLC_OPTS) \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin
	@mkdir -p logs
	@$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES))
	@rm -f test/*.beam

# ------------------------------------------------------------------------------
# dialyzer
# ------------------------------------------------------------------------------
dialyze: $(PLT_FILE)
	@dialyzer +S 8 --src src --plt $(PLT_FILE) --no_native $(DIALYZER_OPTS)

$(PLT_FILE):
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
	$(MAKE) -C $(DOCS_DIR) clean

# ------------------------------------------------------------------------------
# clean project
# ------------------------------------------------------------------------------
distclean: clean clean-docs
	rm -rf $(DEPS_DIR) $(CURDIR)/logs $(CURDIR)/*.beam $(PLT_FILE)
	$(MAKE) -C $(DOCS_DIR) distclean
