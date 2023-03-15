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
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape
    table))

(eval-and-compile
  (defconst silverware-keywords
    '("if" "match" "defun" "load" "defalgebraic" "lambda" "nil"
      "let-in" "let-plus" "forall" "define" "defalias" "defrecord"
      "progn" "#" "@" "!"))
  (defconst silverware-types
    '("Unit" "Integer" "Rational" "String" "Bool" "List" "->"))
  (defconst silverware-kind
    '("Star")))

(defconst silverware-highlights
  `(("//.*" . font-lock-comment-face)
    ("{;\\(.\\|\n\\)*;}" . font-lock-comment-face)
    ("|.*?|" . font-lock-doc-face)        
    (,(regexp-opt silverware-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt silverware-types 'symbols) . font-lock-type-face)
    (,(regexp-opt silverware-kind 'symbols) . font-lock-warning-face)))

(defun silverware-mode-variables ()
  "Setup variables for Silverware mode."
  (setq-local comment-start-skip "{;.*")
  (setq-local comment-end-skip ".*;}")
  (setq-local font-lock-comment-end-skip "\\(;}\\)")
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph))

;;;###autoload
(define-derived-mode silverware-mode prog-mode "Silverware 🥄"
  "Major Mode for editing Silverware source code."
  :syntax-table silverware-mode-syntax-table
  (silverware-mode-variables)
  (setq font-lock-defaults '(silverware-highlights))
  (setq comment-start "//")
  (setq comment-end "")
  (rainbow-delimiters-mode +1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sw\\'" . silverware-mode))
(add-to-list 'auto-mode-alist '("\\.silverware\\'" . silverware-mode))

(provide 'silverware-mode)

;;; silverware-mode.el ends here
