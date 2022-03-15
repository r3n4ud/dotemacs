;; -*- coding: utf-8 -*-
;; Emacs main configuration file
;; Renaud AUBIN

(require 'package)

;; https://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-init"))
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.local/share/emacs/site-lisp/mu4e/")
(progn (cd "~/.emacs.d/site-lisp")
       (normal-top-level-add-subdirs-to-load-path))


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(global-unset-key (kbd "C-c C-t"))
(global-unset-key (kbd "C-t"))

(load-theme 'deeper-blue 'NO-CONFIRM)

;;(if (fboundp 'scroll-bar-mode) (scroll-bar--mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Substitutes the call to yes-or-no-p to y-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Turns on Auto Fill for all modes
(setq-default auto-fill-function 'do-auto-fill)

;; ;; Start emacs in fullscreen mode in Xorg
;; (defun fullscreen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
;; (if (eq window-system 'x)
;;     (add-hook 'emacs-startup-hook 'fullscreen)
;;   )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful before-save-hook bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Updates the time-stamp if present
(add-hook 'before-save-hook 'time-stamp)

;; Removes trailing whitespace from the entire buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Removes tabs from the entire buffer before saving
(add-hook 'before-save-hook
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))))

;; Deletes all blank lines at the end of the file before saving.
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction (widen)
                      (goto-char (point-max))
                      (delete-blank-lines))))
(add-hook 'before-save-hook 'delete-trailing-blank-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indents the whole buffer
(defun iwb ()
  "Indents whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Move line up or down functions
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; PACKAGES

;; those bindings need to by replaced...
(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package smartparens
  :config (smartparens-global-mode 1)
  :ensure t)

(use-package magit
  :ensure t)

(use-package magit-lfs
  :ensure t)

(use-package git-gutter
  :config (global-git-gutter-mode 1)
  :ensure t)

(use-package yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t)

;; Don't use tab as trigger key
(setq yas/trigger-key (kbd "C-c C-t"))
;; (setq yas/snippet-dirs '("~/.emacs.d/site-lisp/snippets"
;;                          "~/.emacs.d/site-lisp/yasnippet/yasmate/snippets"
;;                          "~/.emacs.d/site-lisp/yasnippet/snippets"))
(yas/global-mode 1)

(use-package flycheck
  :ensure t)

(global-flycheck-mode)

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode)
  :ensure t)

(use-package company-quickhelp
  :config (company-quickhelp-mode 1)
  :ensure t)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(use-package company-tabnine :ensure t)
(add-to-list 'company-backends #'company-tabnine)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; (use-package irony
;;   :ensure t)
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package company-irony
  :config (eval-after-load 'company
            '(add-to-list 'company-backends 'company-irony))
  :ensure t)

(use-package company-c-headers
  :config (eval-after-load 'company
            '(add-to-list 'company-backends 'company-c-headers))
  :ensure t)

(add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")

(global-set-key (kbd "C-c y") 'company-yasnippet)
;; http://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))
    ))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


(use-package flycheck-irony
  :ensure t)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package google-c-style
  :ensure t)

(add-hook 'c-mode-common-hook 'google-set-c-style)
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;(require 'flycheck-google-cpplint)
;; (use-package flycheck-google-cpplint
;;   :ensure t)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))

(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint"))

(custom-set-variables
;; '(flycheck-googlelint-verbose "3")
;; '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
;; '(flycheck-googlelint-root "project/src")
 '(flycheck-googlelint-linelength "100"))

;; (require 'xmltok)
;; (require 'init-ido)
;; (require 'init-yasnippet)
;; (require 'init-auto-complete)
;; (require 'init-uniquify)
;; (require 'init-auto-insert)
;; (require 'init-markdown)
;; (require 'init-groovy)
;; (require 'init-js)
;; (require 'init-toml)
;; (require 'init-rust)
;; (require 'init-cmake)

(ido-mode t)
(ido-everywhere t)
;;(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(use-package cmake-font-lock :ensure t)

;; Completion in M-x
(use-package smex
  :config (smex-initialize)
  :ensure t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package markdown-mode   :ensure t)
(use-package toml-mode       :ensure t)

(use-package rust-mode :ensure t)
(setq rust-format-on-save t)

(use-package cargo     :ensure t
  :config
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  )
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'toml-mode-hook 'cargo-minor-mode)

(use-package racer     :ensure t)
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path
      (let* ((sysroot (string-trim
                       (shell-command-to-string "rustc --print sysroot")))
             (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
              (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
        (or (when (file-exists-p lib-path) lib-path)
            (when (file-exists-p src-path) src-path))))
;;(setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(use-package flycheck-rust     :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(use-package company-racer
  :ensure t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

;; built-in
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(auto-insert-mode)
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion
(setq auto-insert-automatically t)

(defun reno/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(custom-set-variables
 '(auto-insert 'other)
 '(auto-insert-directory "~/.emacs.d/site-lisp/auto-insert-templates/")
 '(auto-insert-alist '((("\\.\\([H]\\|hh\\|hpp\\)\\'" . "C++ header") . ["template.hpp" reno/autoinsert-yas-expand])
                       (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cpp" reno/autoinsert-yas-expand])
                       (("\\.rb\\'" . "Ruby script") . ["template.rb" reno/autoinsert-yas-expand])
;                       (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
;                       (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
;                       (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
;                       (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
;                       (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
;                       (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
;                       (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand])
                     )))

(use-package groovy-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-hook 'groovy-mode-hook (lambda() (local-unset-key (kbd "C-d"))))

;; (require 'init-js)

(use-package feature-mode
  :config (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
  :ensure t)

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))
  :ensure t)

(use-package haml-mode      :ensure t)
(use-package coffee-mode    :ensure t)
(use-package yard-mode      :ensure t)
(use-package enh-ruby-mode  :ensure t)
(use-package inf-ruby       :ensure t)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby))

(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
;; (add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-java :ensure t :config (add-hook 'java-mode-hook 'lsp))

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key :ensure t :config (which-key-mode))

(use-package yaml-mode        :ensure t)
(use-package dockerfile-mode  :ensure t)
(use-package lua-mode
  :config (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  :ensure t)

;; Key bindings
(global-set-key [(meta g)] 'goto-line)
;; (global-set-key [(C-right)] 'forward-word) ;; useful for subword-mode
;; (global-set-key [(C-left)] 'backward-word) ;; useful for subword-mode
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(add-hook 'cc-mode-hook
          (lambda()
            (local-unset-key (kbd "C-d"))))

(add-hook 'c++-mode-hook
          (lambda()
            (local-unset-key (kbd "C-d"))))

(add-hook 'c-mode-hook
          (lambda()
            (local-unset-key (kbd "C-d"))))

;; From http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  )
(global-set-key (kbd "C-d") 'duplicate-line)

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Variables configured via the interactive 'customize' interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Use utf-8 as default
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq default-directory "~")
(put 'upcase-region 'disabled nil)

(load "auctex.el" nil t t)

;; (use-package auctex           :ensure t)
;; (use-package tex-site  :ensure auctex)
(use-package company-auctex   :ensure t)

(setq initial-major-mode 'ruby-mode)
(setq initial-scratch-message nil)
(put 'downcase-region 'disabled nil)

(use-package protobuf-mode    :ensure t)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(global-prettify-symbols-mode 1)
;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;;(setq prettify-symbols-unprettify-at-point t)
(require 'fira-code-symbol)

(use-package modern-cpp-font-lock :ensure t)
(modern-c++-font-lock-global-mode t)

(use-package clang-format :ensure t)
(use-package clang-format+ :ensure t)
(add-hook 'c-mode-common-hook #'clang-format+-mode)

(use-package flycheck-clang-tidy
  :after flycheck
  ;; :config
  ;; (setq exec-path (append exec-path '("~/.local/clang+llvm-11.0.1-x86_64-linux-gnu-ubuntu-16.04/bin")))
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )

(use-package adoc-mode :ensure t)


(use-package csv-mode
  :ensure t)


(provide 'init)
;;; init.el ends here
