;; package --- Summary:
;; org-publish.el --- publish related org-mode files as a website
;;; Commentary:
;;;
;;; How to run this:
;;; * eval the buffer
;;; * M-x site-publish
;;;

(require 'org)
(require 'ox-publish)
(require 'ox-html)

;;; org-babel configuration

;;;; don't ask for confirmation before evaluating a code block
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel t)

;;; Project variables:

(defcustom docs-dir "./docs/" "Base directory our docs")
(defcustom out-dir "./public/" "Directory where the HTML files are going to be exported")
(defcustom github-repo "https://github.com/Dr-Nekoma/spatula" "Oficial github repo")

(setq-default root-dir (if (string= (getenv "ENVIRONMENT") "development") (concat (getenv "PWD") "/public") ""))

(setq-default website-html-head (apply 'format (concat
                                                "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\"/>"
                                                "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css\"/>"
                                                "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>"
                                                "<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js\"></script>"
                                                "<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js\"></script>"
                                                "<script type=\"text/javascript\" async src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/latest.js?config=TeX-MML-AM_CHTML\"></script>\n"
                                                "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>\n")
                                       (make-list 7 root-dir)))

(setq-default website-html-postamble (concat "<div class=\"footer\"> Last updated %C."
                                             "<br> Built with %c. "
                                             "Source code availiable "
                                             (concat "<a href=\"" github-repo "\">Here</a>.</div>")))

;;; Code:

(setq org-publish-project-alist
      `(("docs"
         :base-directory ,docs-dir
         :base-extension "org"
         :publishing-directory ,out-dir
         :publishing-function org-html-publish-to-html

         :recursive t

         :export-with-tags nil
         :exclude-tags ("todo" "noexport")
         :exclude "level-.*\\|.*\.draft\.org"
         :with-title t
         :section-numbers nil
         :headline-levels 5
         :with-toc nil
         :with-date t

         :auto-sitemap nil
         :sitemap-filename "index.org"
         :sitemap-title "Home"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d - %t"
         :sitemap-style list

         :html-doctype "html5"
         :html-html5-fancy t
         :html-head ,website-html-head
         :html-postamble ,website-html-postamble)

        ("examples"
         :base-directory "./examples/"
         :base-extension "sw"
         :publishing-directory ,(concat out-dir "examples")
         :recursive t
         :publishing-function org-publish-attachment)

        ("all" :components ("docs" "examples"))))
