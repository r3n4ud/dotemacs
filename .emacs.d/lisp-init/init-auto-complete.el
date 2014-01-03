(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(ac-config-default)
(setq-default ac-sources '(ac-source-yasnippet ac-sources))

(defun ac-custom-lisp-mode-setup ()
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-features
                     ac-source-functions
                     ac-source-variables
                     ac-source-abbrev
                     ac-source-symbols
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers))
  ((lambda () (auto-complete-mode 1))))
(add-hook 'emacs-lisp-mode-hook 'ac-custom-lisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook 'ac-custom-lisp-mode-setup)
(add-hook 'lisp-mode-hook 'ac-custom-lisp-mode-setup)

;; android specific settings
;; AndroidManifest.xml
(defun ac-android-manifest-nxml-setup()
  (when (string= (buffer-name) "AndroidManifest.xml")
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-abbrev
                       ac-source-dictionary
                       ac-source-words-in-same-mode-buffers))
    ((lambda () (auto-complete-mode 1)))))
(add-hook 'nxml-mode-hook 'ac-android-manifest-nxml-setup)

(provide 'init-auto-complete)
