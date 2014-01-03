(ido-mode t)
(ido-everywhere t)
;;(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; Completion in M-x
;; (smex-initialize)
;; (global-set-key "\M-x" 'smex)

(provide 'init-ido)
