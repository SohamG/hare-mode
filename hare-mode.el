;;; hare-mode.el --- Hare mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamín Buccianti
;; Copyright (C) 2020 Amin Bandali

;; Author: Benjamín Buccianti <benjamin@buccianti.dev>
;;         Amin Bandali <bandali@gnu.org>
;; Keywords: languages
;; URL: https://git.sr.ht/~bbuccianti/hare-mode
;; Version: 0.1.0

;; Hare mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Hare mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Hare mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing `hare' files in GNU Emacs.

;;; Code:

(defconst hare-mode--regexp-declaration-line-beginning
  (concat "^" (regexp-opt hare-mode-keywords))
  "Regexp matching `hare-mode-keywords' on line beginning.")

(defconst hare-mode--regexp-declaration-end
  (regexp-opt '("};" ");"))
  "Regexp matching declaration endings.")


(defvar hare-mode-map
   (let ((map (make-sparse-keymap)))
     map)
   "Keymap for `hare-mode'.")

(defvar hare-mode-keywords
  '("as" "break" "const" "continue" "def" "else" "export" "fn" "for"
    "if" "is" "let" "match" "return" "size" "static" "switch" "use"
    "while"))

(defvar hare-mode-types
  '("u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64" "int" "uint"
    "uintptr" "f32" "f64" "bool" "char" "str" "void" "struct" "union"
    "nullable"))

(defvar hare-mode-constants
  '("null" "true" "false"))

(defvar hare-mode-builtins
  '("@init" "@symbol" "@test" "@fini" "len" "offset" "free" "alloc"
    "assert"))

(defvar hare-mode-font-lock-defaults
  `((("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt hare-mode-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt hare-mode-constants 'symbols) . font-lock-constant-face)
     (,(regexp-opt hare-mode-builtins 'symbols) . font-lock-builtin-face)
     (,(regexp-opt hare-mode-types 'symbols) . font-lock-type-face))))

(defconst hare-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; strings and characters
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; comments
    (modify-syntax-entry ?/  ". 14b" st)
    (modify-syntax-entry ?*  ". 23n" st)

    ;; @ is part of symbols in Hare
    (modify-syntax-entry ?@ "_" st)

    ;; return our modified syntax table
    st))

(defvar hare-mode-imenu-generic-expression
  `(;; Functions
    (nil
     ,(concat
       (rx line-start)
       (rx (opt "export" (1+ blank)))
       (rx (opt "@" (or "test" "init" "fini") (1+ blank)))
       (rx (opt "@symbol(" (regexp "\".*\"") ")" (1+ blank)))
       (rx "fn" (1+ blank))
       (rx (group (or letter "_") (0+ (or letter "_" digit)))) ;; identifier
       (rx (0+ (syntax whitespace)))
       ;; Optional parameter list
       (rx (opt (syntax open-parenthesis)
		(0+ (any letter ":" "*" "," "_" "[" "]" digit whitespace))
		(syntax close-parenthesis)))
       (rx (0+ (syntax whitespace)))
       (rx (opt (1+ letter)))  ;; Optional nullable
       (rx (0+ (syntax whitespace)))
       (rx (opt (1+ letter)))  ;; Optional const
       (rx (0+ (syntax whitespace)))
       (rx (opt (or "*" "&")) (or (1+ (any letter ":")))) ;; result type
       (rx (0+ (syntax whitespace)))
       "="
       ) 1)))

(defun hare-mode-beginning-of-defun (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (re-search-backward hare-mode--regexp-declaration-line-beginning nil t arg))

(defun hare-mode-end-of-defun (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (re-search-forward hare-mode--regexp-declaration-end nil t arg))

;;;###autoload
(define-derived-mode hare-mode prog-mode "Hare"
  "Major mode for editing `hare' files."
  :syntax-table hare-mode-syntax-table

  (setq-local beginning-of-defun-function #'hare-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'hare-mode-end-of-defun)

  (setq-local font-lock-defaults hare-mode-font-lock-defaults)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq imenu-generic-expression hare-mode-imenu-generic-expression)
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode))

(provide 'hare-mode)
;;; hare-mode.el ends here
