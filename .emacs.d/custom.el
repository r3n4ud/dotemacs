;;; package --- Summary
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "~/.local/opt/firefox/firefox")
 '(browse-url-generic-program (executable-find "chromium"))
 '(clang-format-executable "~/.local/opt/clang/bin/clang-format")
 '(flycheck-c/c++-clang-executable "~/.local/opt/clang/bin/clang++")
 '(flycheck-c/c++-clang-tidy-executable "~/.local/opt/clang/bin/clang-tidy")
 '(flycheck-c/c++-gcc-executable "/usr/bin/gcc")
 '(flycheck-c/c++-googlelint-executable "~/.local/bin/cpplint")
 '(flymake-proc-allowed-file-name-masks
   '(("\\.xml\\'" flymake-proc-xml-init nil nil)
     ("\\.html?\\'" flymake-proc-xml-init nil nil)
     ("\\.cs\\'" flymake-proc-simple-make-init nil nil)
     ("\\.php[345]?\\'" flymake-proc-php-init nil nil)
     ("[0-9]+\\.tex\\'" flymake-proc-master-tex-init flymake-proc-master-cleanup nil)
     ("\\.tex\\'" flymake-proc-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-proc-simple-make-init nil nil)))
 '(lsp-java-format-settings-url
   "file:///REPLACEWITHHOME/.emacs.d/eclipse-java-google-style.xml")
 '(lsp-java-java-path "~/.local/opt/java/jdk-17.0.6+10/bin/java")
 '(lsp-java-server-install-dir "~/.local/opt/java/jdt-language-server-1.9.0/")
 '(lsp-pylsp-plugins-flake8-enabled nil)
 '(lsp-pylsp-plugins-flake8-exclude [])
 '(lsp-pylsp-plugins-flake8-filename [])
 '(lsp-pylsp-plugins-pydocstyle-enabled nil)
 '(lsp-pylsp-plugins-ruff-enabled t)
 '(lsp-pylsp-plugins-ruff-format [])
 '(lsp-pylsp-plugins-ruff-ignore [])
 '(lsp-pylsp-plugins-ruff-line-length 88)
 '(lsp-pylsp-plugins-ruff-preview t)
 '(max-lisp-eval-depth 2000)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(emojify ivy-rich lsp-docker treemacs-magit treemacs-icons-dired treemacs-all-the-icons treemacs yard-mode company-inf-ruby company-nginx elisp-refs helpful ein rustic counsel company-box pyenv-mode lsp-ui lsp-java flycheck-clang-analyzer flycheck-clang-tidy mu4e-alert csv-mode auctex org-mime adoc-mode clang-format+ clang-format modern-cpp-font-lock yasnippet-snippets company-irony-c-headers ess-view ess-smart-equals ess 2048-game plantuml-mode protobuf-mode flycheck-irony cmake-font-lock yasnippet company-auctex lua-mode yaml-mode rust-mode use-package undo-tree smex smartparens git-gutter company-quickhelp))
 '(pixel-scroll-precision-mode t)
 '(prettify-symbols-unprettify-at-point t)
 '(rust-rustfmt-bin "~/.cargo/bin/rustfmt")
 '(safe-local-variable-values '((whitespace-line-column . 80)))
 '(warning-suppress-types '(("terst"))))
(provide 'custom)
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-icons
 ;; custom-set-icons was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(warnings-suppress ((emoji "⛔") (symbol "⛔") (text " stop "))))
