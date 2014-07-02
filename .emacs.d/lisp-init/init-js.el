(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-web-server")
(add-to-list 'load-path "~/.emacs.d/site-lisp/skewer-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/js2-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ac-js2")

(require 'ac-js2)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(provide 'init-js)
