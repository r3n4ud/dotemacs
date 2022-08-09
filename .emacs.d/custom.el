;;; package --- Summary
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "~/.local/opt/firefox/firefox")
 '(browse-url-generic-program (executable-find "chromium"))
 '(clang-format-executable
   "~/.local/clang+llvm-11.0.1-x86_64-linux-gnu-ubuntu-16.04/bin/clang-format")
 '(flycheck-c/c++-googlelint-executable "~/.local/bin/cpplint")
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(company-box pyenv-mode lsp-ui lsp-java flycheck-clang-analyzer flycheck-clang-tidy mu4e-alert csv-mode auctex org-mime adoc-mode clang-format+ clang-format modern-cpp-font-lock yasnippet-snippets company-irony-c-headers ess-view ess-smart-equals ess flycheck-rust 2048-game plantuml-mode protobuf-mode flycheck-irony cmake-font-lock yasnippet company-auctex lua-mode yaml-mode racer cargo rust-mode use-package undo-tree smex smartparens magit git-gutter company-quickhelp))
 '(prettify-symbols-unprettify-at-point t)
 '(safe-local-variable-values '((whitespace-line-column . 80)))
 '(show-paren-mode t)
 '(smtpmail-smtp-server "localhost")
 '(smtpmail-smtp-service 25))
(provide 'custom)
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
