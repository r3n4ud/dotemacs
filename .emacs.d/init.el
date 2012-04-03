;; -*- coding: utf-8 -*-
;; Emacs main configuration file
;; Renaud AUBIN
;; Time-stamp: <2012-04-03 18:58:43>

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Substitutes the call to yes-or-no-p to y-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Turns on Auto Fill for all modes
(setq-default auto-fill-function 'do-auto-fill)

;; Start emacs in fullscreen mode in Xorg
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(if (eq window-system 'x)
    (add-hook 'emacs-startup-hook 'fullscreen)
)

(require 'autopair)
(autopair-global-mode)

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


;; Modes configuration
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(progn (cd "~/.emacs.d/site-lisp")
       (normal-top-level-add-subdirs-to-load-path))

(require 'xmltok)
(require 'init-ido)
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-uniquify)
(require 'init-auto-insert)

;; rinari
(require 'rinari)

;; ;; nXhtml
;; (load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; ;; Enforce nxml mode for xml file
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . auto-complete-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

(require 'custom-java-style)
(add-hook 'java-mode-hook 'custom-make-newline-indent)
(add-hook 'java-mode-hook 'custom-set-java-style)

(require 'android-mode)

(require 'magit)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

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
