(require 'yasnippet)
;; Don't use tab as trigger key
(setq yas/trigger-key (kbd "C-c C-t"))
(setq yas/snippet-dirs '("~/.emacs.d/site-lisp/snippets" "~/.emacs.d/site-lisp/yasnippet/snippets" "~/.emacs.d/site-lisp/yasnippet/extras/imported"))
(yas/global-mode 1)

(provide 'init-yasnippet)
