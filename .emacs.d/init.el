;; Emacs main configuration file
;; Renaud AUBIN
;; Time-stamp: <2012-02-17 14:08:17>
;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Substitutes the call to yes-or-no-p to y-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Turns on Auto Fill for all modes
;; TODO: check if this is redundant with (auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful before-save-hook bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Updates the time-stamp if present
(add-hook 'before-save-hook 'time-stamp)

;; Removes trailing whitespace from the entire buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Removes tabs from the entire buffer before saving
(add-hook 'before-save-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))))

;; Deletes all blank lines at the end of the file before saving.
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction (widen)
                      (goto-char (point-max))
                      (delete-blank-lines))))
(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indents the whole buffer
;; Original
;; (defun iwb ()
;;   "indent whole buffer"
;;   (interactive)
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max)))
;; Mine
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



;; Set the color theme (deprecated in emacs 24)
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-charcoal-black)

;; rinari
(add-to-list 'load-path "~/.emacs.d/site-lisp/rinari")
(require 'rinari)

;; nXhtml
(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; ;; Enforce nxml mode for xml file
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . auto-complete-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c C-t"))
(setq yas/root-directory '("~/.emacs.d/site-lisp/yasnippet/snippets"
                           "~/.emacs.d/site-lisp/snippets"))
;;                           "~/.emacs.d/site-lisp/external-snippets"))
(yas/initialize)
;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(ac-config-default)

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

;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(add-to-list 'load-path "~/.emacs.d/site-lisp/custom-java-style")
(require 'custom-java-style)
(add-hook 'java-mode-hook 'custom-make-newline-indent)
(add-hook 'java-mode-hook 'custom-set-java-style)

(add-to-list 'load-path "/home/renaud/.emacs.d/site-lisp/android-mode")
(require 'android-mode)

(require 'magit)

(add-to-list 'load-path "/home/renaud/.emacs.d/site-lisp/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-to-list 'load-path "/home/renaud/.emacs.d/site-lisp/rvm")
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(add-to-list 'load-path "/home/renaud/.emacs.d/site-lisp/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ajc-java-complete/")
;; (require 'ajc-java-complete-config)
;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
;; (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; ;; subword mode is usefull to navigate within StringLikeThisOne
;; (subword-mode 1)
;; (add-hook 'c++-mode-hook (lambda () (subword-mode 1)))
;; (add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))
;; (add-hook 'cmake-mode-hook (lambda () (subword-mode 1)))
;; (add-hook 'text-mode-hook (lambda () (subword-mode 1)))
;; (add-hook 'nxml-mode-hook (lambda () (subword-mode 1)))
;; (add-hook 'java-mode-hook (lambda () (subword-mode 1)))
;; ;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
;; ;; (add-hook 'c-mode-common-hook
;; ;;               (lambda () (subword-mode 1)))
;; (add-hook 'c-mode-common-hook 'c-subword-mode)


;; Key bindings
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(C-right)] 'forward-word) ;; useful for subword-mode
(global-set-key [(C-left)] 'backward-word) ;; useful for subword-mode
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
