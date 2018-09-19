(add-to-list 'load-path "elisp")
(add-to-list 'load-path "elisp/org-mode/lisp")

(require 'ox-publish)

(setq org-publish-project-alist
      '(
        ("leptus-docs"
         :base-directory "./org"
         :base-extension "org"
         :publishing-directory "./public_html"
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :recursive nil
         :with-title t
         :with-author t
         :with-footnotes t
         :with-timestamps t
         :with-emphasize t
         :with-sub-superscript nil
         :section-numbers nil
         :html-validation-link nil
         :html-html5-fancy t
         :html-doctype "<!DOCTYPE html>"
         :html-link-up "index.html"
         :html-link-home "https://github.com/sinasamavati/leptus"
         :html-metadata-timestamp-format "%Y-%m-%d"
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"style.css\">"
         )))

(org-publish "leptus-docs")
