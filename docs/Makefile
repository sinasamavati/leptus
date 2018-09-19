.PHONY: all deps clean distclean

ELISP_DIR = $(CURDIR)/elisp
PUBLISH_SCRIPT = $(CURDIR)/publish.el

ORG_MODE_DIR = $(ELISP_DIR)/org-mode
ORG_MODE_URL = http://orgmode.org/org-9.1.14.tar.gz
ORG_MODE = $(ELISP_DIR)/org-9.1.14.tar.gz

all: deps public_html/style.css
	emacs -Q --script $(PUBLISH_SCRIPT)

public_html/%.css: css/%.css | $(CURDIR)/public_html
	cat $< | sed -e 's/^[ \t]*//' \
		     -e 's/[ \t]*{/{/' \
		     -e 's/:[ \t]/:/' \
		| tr -d '\n' > $@

deps: $(ORG_MODE)

$(ORG_MODE): | $(ORG_MODE_DIR)
	curl -L $(ORG_MODE_URL) -o $(ORG_MODE)
	cd $(ORG_MODE_DIR); tar xzf $(ORG_MODE) --strip-components=1
	$(MAKE) -C $(ORG_MODE_DIR);

$(CURDIR)/%:
	mkdir -p $@

clean:
	rm -rf $(CURDIR)/public_html

distclean: clean
	rm -rf $(ELISP_DIR)
