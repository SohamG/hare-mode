(defvar hare-keywords
  '("alloc" "assert" "as" "break" "const" "continue" "def" "else" "export"
    "fn" "for" "free" "if" "is" "len" "let" "match" "return" "size" "static"
    "switch" "use" "while" "u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64" "int"
    "uint" "uintptr" "f32" "f64" "bool" "char" "str" "void" "struct" "union"
    "nullable" "null" "true" "false"))

;;;###autoload
(define-derived-mode hare-mode prog-mode "Hare"
  "Major mode for editing `hare' files."

  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode))

(provide 'hare-mode)
