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
 '(clang-format-executable "~/.local/clang/bin/clang-format")
 '(flycheck-c/c++-clang-executable "~/.local/clang/bin/clang++")
 '(flycheck-c/c++-clang-tidy-executable "~/.local/clang/bin/clang-tidy")
 '(flycheck-c/c++-gcc-executable "/opt/gcc-8.3.0/bin/gcc")
 '(flycheck-c/c++-googlelint-executable "~/.local/bin/cpplint")
 '(lsp-java-format-settings-url
   "file:///REPLACEWITHHOME/.emacs.d/eclipse-java-google-style.xml")
 '(lsp-java-java-path "/usr/lib/jvm/jdk-11/bin/java")
 '(lsp-java-server-install-dir "~/.local/opt/jdt-language-server-1.5.0/")
 '(magit-lfs-git-lfs-executable "~/.local/bin/git-lfs")
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(yard-mode company-inf-ruby company-nginx elisp-refs helpful ein rustic counsel company-box pyenv-mode lsp-ui lsp-java flycheck-clang-analyzer flycheck-clang-tidy mu4e-alert csv-mode auctex org-mime adoc-mode clang-format+ clang-format modern-cpp-font-lock yasnippet-snippets company-irony-c-headers ess-view ess-smart-equals ess 2048-game plantuml-mode protobuf-mode flycheck-irony cmake-font-lock yasnippet company-auctex lua-mode yaml-mode rust-mode use-package undo-tree smex smartparens git-gutter company-quickhelp))
 '(prettify-symbols-unprettify-at-point t)
 '(rust-rustfmt-bin "~/.cargo/bin/rustfmt")
 '(safe-local-variable-values '((whitespace-line-column . 80))))
(provide 'custom)
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
