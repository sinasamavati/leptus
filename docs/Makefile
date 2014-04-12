.PHONY: all clean

TMP_DIR = $(CURDIR)/tmp
INDEX_FILE = $(TMP_DIR)/index.org
PUBLISH_SCRIPT = $(CURDIR)/publish.el

all: $(TMP_DIR)
	cp -r *.org $(TMP_DIR)
	sed 's/\(.*[^/]\)\([0-9]\)/\1-\2/g;s/.org#/.org::/g;s/\([^\[]*.org\)/file:\1/g' \
		-i $(INDEX_FILE)
	emacs -Q --script $(PUBLISH_SCRIPT)

clean:
	rm -rf $(CURDIR)/public_html $(TMP_DIR)

$(TMP_DIR):
	mkdir -p $(TMP_DIR)
