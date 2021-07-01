;;; package --- Summary
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-fill-mode t)
 '(auto-insert 'other)
 '(auto-insert-alist
   '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
      .
      ["template.h" c++-mode reno/autoinsert-yas-expand])
     (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source")
      .
      ["template.cpp" reno/autoinsert-yas-expand])
     (("\\.rb\\'" . "Ruby script")
      .
      ["template.rb" reno/autoinsert-yas-expand])))
 '(auto-insert-directory "~/.emacs.d/site-lisp/auto-insert-templates/")
 '(backup-directory-alist '(("." . "~/.emacs-backup-files/")))
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "~/.local/opt/firefox/firefox")
 '(browse-url-generic-program (executable-find "chromium"))
 '(clang-format-executable
   "~/.local/clang+llvm-11.0.1-x86_64-linux-gnu-ubuntu-16.04/bin/clang-format")
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(diary-file "~/org/diary")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-format "∥ %A %e %B − %R ∥")
 '(display-time-mode t)
 '(fill-column 100)
 '(flycheck-c/c++-googlelint-executable "~/.local/bin/cpplint")
 '(flycheck-googlelint-linelength "100")
 '(fringe-mode nil nil (fringe))
 '(git-commit-fill-column 72)
 '(git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-turn-on-flyspell git-commit-propertize-diff with-editor-usage-message))
 '(git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
 '(git-commit-summary-max-length 51)
 '(global-font-lock-mode t)
 '(global-subword-mode t)
 '(icomplete-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(iswitchb-mode nil)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(mu4e-alert csv-mode auctex org-mime adoc-mode clang-format+ clang-format modern-cpp-font-lock yasnippet-snippets company-irony-c-headers ess-view ess-smart-equals ess flycheck-rust 2048-game plantuml-mode protobuf-mode flycheck-irony cmake-font-lock yasnippet company-auctex lua-mode yaml-mode racer cargo rust-mode use-package undo-tree smex smartparens magit git-gutter company-quickhelp))
 '(plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(plantuml-java-args '("-Djava.awt.headless=true" "-jar"))
 '(plantuml-java-command "java")
 '(prettify-symbols-unprettify-at-point t)
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((whitespace-line-column . 80)))
 '(show-paren-mode t)
 '(smtpmail-smtp-server "localhost")
 '(smtpmail-smtp-service 25)
 '(time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
 '(tool-bar-mode nil)
 '(whitespace-line-column 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Lucida Grande Mono DK" :foundry "b&h" :slant normal :weight normal :height 113 :width semi-condensed)))))
