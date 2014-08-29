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
# AUTHORS OR COPYRIGHT oOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

PROJECT = leptus
CT_SUITES = leptus_router leptus_req leptus_handler leptus_http leptus_pt \
	leptus_config leptus_logger

DEPS = cowboy msgpack

dep_cowboy = git https://github.com/extend/cowboy 0.9.0
dep_msgpack = git https://github.com/msgpack/msgpack-erlang/ 0.2.8

ifdef USE_JSX
    dep_jsx = git https://github.com/talentdeficit/jsx v1.4.5
    DEPS += jsx
    ERLC_OPTS += -DUSE_JSX
else
    dep_jiffy = git https://github.com/davisp/jiffy 0.8.5
    DEPS += jiffy
endif

include erlang.mk

# ------------------------------------------------------------------------------
# run erlang shell
# ------------------------------------------------------------------------------
shell:
	erl -pa $(EBIN) $(DEPS_DIR)/*/ebin
