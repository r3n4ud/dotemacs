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

(use-package company-racer
  :ensure t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

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

(use-package cargo     :ensure t)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'toml-mode-hook 'cargo-minor-mode)

(use-package racer     :ensure t)
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(use-package flycheck-rust     :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

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

;; ;; (use-package robe           :ensure t)

;; (eval-after-load 'company
;;   '(push 'company-robe company-backends))

;; (add-hook 'after-init-hook 'inf-ruby-switch-setup)


;; ;; ;; rinari
;; ;; (require 'rinari)
;; (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))
;; ;; (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
;; ;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)



;; ;; ;; nXhtml
;; ;; (load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
;; ;; (setq
;; ;;  nxhtml-global-minor-mode t
;; ;;  mumamo-chunk-coloring 'submode-colored
;; ;;  nxhtml-skip-welcome t
;; ;;  indent-region-mode t
;; ;;  rng-nxml-auto-validate-flag nil
;; ;;  nxml-degraded t)
;; ;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; ;; ;; Enforce nxml mode for xml file
;; ;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . auto-complete-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


(add-to-list 'load-path "~/.emacs.d/site-lisp/custom-java-style")
;; (progn (cd "~/.emacs.d/site-lisp")
;;        (normal-top-level-add-subdirs-to-load-path))

(require 'custom-java-style)
(add-hook 'java-mode-hook 'custom-make-newline-indent)
(add-hook 'java-mode-hook 'custom-set-java-style)
(add-hook 'java-mode-hook (lambda() (local-unset-key (kbd "C-d"))))

;; (require 'android-mode)

(use-package yaml-mode        :ensure t)
(use-package dockerfile-mode  :ensure t)
(use-package lua-mode
  :config (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  :ensure t)

(use-package ess
  :ensure t
  :defer t
;;  :pre-load (setq ess-R-smart-operators t) ; enables smart commas too
  :init (autoload 'R-mode "ess-site"
          "Major mode for editing R source.  See `ess-mode' for more help."
          t)
  :config (progn
            (eval-after-load 'ess-smart-equals
              '(progn
                 (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
                 (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode)))
            (use-package ess-smart-equals :ensure t)))

(setq ess-R-smart-operators t) ; enables smart commas too
;;(use-package ess-R-data-view   :ensure t)
(use-package ess-view   :ensure t)

;; ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ajc-java-complete/")
;; ;; (require 'ajc-java-complete-config)
;; ;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
;; ;; (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; ;; ;; subword mode is usefull to navigate within StringLikeThisOne
;; ;; (subword-mode 1)
;; ;; (add-hook 'c++-mode-hook (lambda () (subword-mode 1)))
;; ;; (add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))
;; ;; (add-hook 'cmake-mode-hook (lambda () (subword-mode 1)))
;; ;; (add-hook 'text-mode-hook (lambda () (subword-mode 1)))
;; ;; (add-hook 'nxml-mode-hook (lambda () (subword-mode 1)))
;; ;; (add-hook 'java-mode-hook (lambda () (subword-mode 1)))
;; ;; ;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
;; ;; ;; (add-hook 'c-mode-common-hook
;; ;; ;;               (lambda () (subword-mode 1)))
;; ;; (add-hook 'c-mode-common-hook 'c-subword-mode)

;; groovy, java, C and related modes
;; (defun my-c-mode-hook ()
;;   (setq indent-tabs-mode nil
;;         c-basic-offset 4))
;; (add-hook 'c-mode-common-hook 'my-c-mode-hook)

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

;; (defun nxml-compute-indent-in-start-tag (pos)
;;   "Return the indent for a line that starts inside a start-tag.
;; Also for a line that starts inside an empty element.
;; POS is the position of the first non-whitespace character of the line.
;; This expects the xmltok-* variables to be set up as by `xmltok-forward'."
;;   (let ((value-boundary (nxml-attribute-value-boundary pos))
;;         (off 0))
;;     (if value-boundary
;;         ;; inside an attribute value
;;         (let ((value-start (car value-boundary)))
;;           (goto-char pos)
;;           (forward-line -1)
;;           (if (< (point) value-start)
;;               (goto-char value-start)
;;             (back-to-indentation)))
;;       ;; outside an attribute value
;;       (goto-char (+ pos 1))
;;       (back-to-indentation)
;;       ;; (while (and (= (forward-line -1) 0)
;;       ;;             (nxml-attribute-value-boundary (point))))
;;       ;; (cond ((<= (point) xmltok-start)
;;       ;;        (goto-char xmltok-start)
;;       ;;        (setq off nxml-attribute-indent)
;;       ;;        (let ((atts (xmltok-merge-attributes)))
;;       ;;          (when atts
;;       ;;            (let* ((att (car atts))
;;       ;;                   (start (xmltok-attribute-name-start att)))
;;       ;;              (when (< start pos)
;;       ;;                (goto-char start)
;;       ;;                (setq off 0))))))
;;       ;;       (t
;;       ;;        (back-to-indentation)))
;;       )
;;     (+ (current-column) off)))

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

(use-package adoc-mode :ensure t)


(use-package csv-mode
  :ensure t)


(provide 'init)
;;; init.el ends here
