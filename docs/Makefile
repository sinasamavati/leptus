.PHONY: all deps clean distclean

TMP_DIR = $(CURDIR)/tmp
INDEX_FILE = $(TMP_DIR)/index.org
PUBLISH_SCRIPT = $(CURDIR)/publish.el

DEPS_DIR = $(CURDIR)/elisp

ORG_MODE_DIR = $(DEPS_DIR)/org-mode
ORG_MODE_URL = http://orgmode.org/org-8.2.10.tar.gz
ORG_MODE = $(DEPS_DIR)/org-8.2.10.tar.gz

all: deps $(TMP_DIR)
	cp -r *.org $(TMP_DIR)
	sed 's/\(.*[^/]\)\([0-9]\)/\1-\2/g;s/.org#/.org::/g;s/\([^\[]*.org\)/file:\1/g' \
		-i $(INDEX_FILE)
	emacs -Q --script $(PUBLISH_SCRIPT)

deps: $(ORG_MODE_DIR) $(ORG_MODE)

$(ORG_MODE):
	curl -L $(ORG_MODE_URL) -o $(ORG_MODE)
	cd $(ORG_MODE_DIR); tar xzf $(ORG_MODE) --strip-components=1
	-cd $(ORG_MODE_DIR); $(MAKE)

$(CURDIR)/%:
	mkdir -p $@

clean:
	rm -rf $(CURDIR)/public_html $(TMP_DIR)

distclean: clean
	rm -rf $(DEPS_DIR)
