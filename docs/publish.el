(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("leptus-docs"
         :base-directory "./tmp"
         :base-extension "org"
         :publishing-directory "./public_html"
         :recursive t
         :publishing-function org-publish-org-to-html
         :exclude "README.org"
         :headline-levels 4
         :auto-preamble t
         )
        ))
(setq org-link-search-must-match-exact-headline nil)
(org-publish "leptus-docs" t)
