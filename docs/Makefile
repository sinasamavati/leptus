.PHONY: all deps clean distclean

TMP_DIR = $(CURDIR)/tmp
INDEX_FILE = $(TMP_DIR)/index.org

ELISP_DIR = $(CURDIR)/elisp
PUBLISH_SCRIPT = $(ELISP_DIR)/publish.el

ORG_MODE_DIR = $(ELISP_DIR)/org-mode
ORG_MODE_URL = http://orgmode.org/org-9.1.14.tar.gz
ORG_MODE = $(ELISP_DIR)/org-9.1.14.tar.gz

all: deps $(TMP_DIR)
	cp -r *.org $(TMP_DIR)
	find $(TMP_DIR) -name "*.org" -exec \
		sed -e 's/#+BEGIN_SRC erlang/#+BEGIN_SRC/;' \
		    -e 's/\(.*[^/]\)\([0-9]\)/\1-\2/;' \
		    -e 's/.org#/.org::/;' \
		    -e 's/\([^\[]*.org\)/file:\1/g' -i \
		{} \;
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
	rm -rf $(ORG_MODE) $(ORG_MODE_DIR)
