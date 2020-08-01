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

(defvar hare-mode-map
   (let ((map (make-sparse-keymap)))
     map)
   "Keymap for `hare-mode'.")

(defvar hare-mode-keywords
  '("alloc" "assert" "as" "break" "const" "continue" "def" "else"
    "export" "fn" "for" "free" "if" "is" "len" "let" "match" "return"
    "size" "static" "switch" "use" "while" "u8" "u16" "u32" "u64" "i8"
    "i16" "i32" "i64" "int" "uint" "uintptr" "f32" "f64" "bool" "char"
    "str" "void" "struct" "union" "nullable"))

(defvar hare-mode-constants
  '("null" "true" "false"))

(defvar hare-mode-font-lock-keywords
  `((,(regexp-opt hare-mode-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt hare-mode-constants 'symbols) . font-lock-constant-face)))

;;;###autoload
(define-derived-mode hare-mode prog-mode "Hare"
  "Major mode for editing `hare' files."

  (setq-local font-lock-defaults '(hare-mode-font-lock-keywords))
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode))

(provide 'hare-mode)
;;; hare-mode.el ends here
