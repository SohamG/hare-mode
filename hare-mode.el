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
