(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)$" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(provide 'init-markdown)
