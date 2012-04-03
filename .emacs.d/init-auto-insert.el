;;(require 'auto-insert-mode)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/site-lisp/auto-insert-templates/")
;; (setq auto-insert-query nil)
(define-auto-insert "\.rb" "ruby-template.rb")

(provide 'init-auto-insert)
