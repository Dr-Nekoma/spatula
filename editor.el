;;; silverware-mode.el --- Major Mode for editing Porth source code -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Major Mode for editing Silverware source code.
;;; Code:

(require 'rainbow-delimiters)

(defvar silverware-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)
    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Delimiters
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Prefix chars
    (modify-syntax-entry ?# "'" table)
    (modify-syntax-entry ?! "'" table) ; => #[1 2 ![+ 1 2]]
    (modify-syntax-entry ?@ "'" table)
    ;; Others
    (modify-syntax-entry ?\;  "< 123" table)
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape
    table))

(eval-and-compile
  (defconst silverware-keywords
    '("if" "match" "defun" "do" "open" "defalgebraic" "defmacro" "lambda" "and" "or"
      "+" "-" "*" "/" "=:=" ">" "<" ">=" "<=" "not"
      "defrecord" "begin" "call" "->" "rec" "list" "quote" "#" "@" "!")))

(defconst silverware-highlights
  `((,(regexp-opt silverware-keywords 'symbols) . font-lock-keyword-face)))

(defun silverware-mode-variables ()
  "Setup variables for Silverware mode."
  (setq-local comment-start-skip
	      "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\||#\\)")
  (setq-local font-lock-comment-end-skip "|#")
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph))

;;;###autoload
(define-derived-mode silverware-mode prog-mode "Silverware 🥄"
  "Major Mode for editing Silverware source code."
  :syntax-table silverware-mode-syntax-table
  (setq font-lock-defaults '(silverware-highlights))
  (setq-local comment-start "// ")
  (rainbow-delimiters-mode +1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.silw\\'" . silverware-mode))
(add-to-list 'auto-mode-alist '("\\.sw\\'" . silverware-mode))
(add-to-list 'auto-mode-alist '("\\.silverware\\'" . silverware-mode))

(provide 'silverware-mode)

;;; silverware-mode.el ends here
