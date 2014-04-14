.PHONY: all deps clean distclean

TMP_DIR = $(CURDIR)/tmp
INDEX_FILE = $(TMP_DIR)/index.org
PUBLISH_SCRIPT = $(CURDIR)/publish.el

DEPS_DIR = $(CURDIR)/elisp
ORG_MODE_URL = http://orgmode.org/org-latest.tar.gz
ORG_MODE = $(DEPS_DIR)/org-latest.tar.gz
HTMLIZE_URL = http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi
HTMLIZE = $(DEPS_DIR)/htmlize.el
DEPS = $(ORG_MODE) \
       $(HTMLIZE)

all: deps $(TMP_DIR)
	cp -r *.org $(TMP_DIR)
	sed 's/\(.*[^/]\)\([0-9]\)/\1-\2/g;s/.org#/.org::/g;s/\([^\[]*.org\)/file:\1/g' \
		-i $(INDEX_FILE)
	emacs -Q --script $(PUBLISH_SCRIPT)

deps: $(DEPS_DIR) $(DEPS)

clean:
	rm -rf $(CURDIR)/public_html $(TMP_DIR)

distclean: clean
	rm -rf $(DEPS_DIR)

$(TMP_DIR):
	mkdir -p $(TMP_DIR)

$(DEPS_DIR):
	mkdir -p $(DEPS_DIR)

$(ORG_MODE):
	wget $(ORG_MODE_URL) -O $(ORG_MODE)
	cd $(DEPS_DIR); tar xzf $(ORG_MODE)
	cd $(DEPS_DIR)/org-mode; $(MAKE) compile

$(HTMLIZE):
	wget $(HTMLIZE_URL) -O $(HTMLIZE)
