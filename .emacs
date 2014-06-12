;; EQC Emacs Mode -- Configuration Start
(add-to-list 'load-path "/usr/local/stow/erlang-r14b02/lib/erlang/lib/eqc-1.24.3/emacs/")
(autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
(add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
(setq eqc-max-menu-length 30)
(setq eqc-root-dir "/usr/local/stow/erlang-r14b02/lib/erlang/lib/eqc-1.24.3")
;; EQC Emacs Mode -- Configuration End

;; Erlang Emacs Mode -- Configuration Start
(setq erlang-root-dir "/usr/local/stow/erlang-r14b02/lib/erlang")
(setq load-path (cons "/usr/local/stow/erlang-r14b02/lib/erlang/lib/tools-2.6.6.3/emacs" load-path))
(setq exec-path (cons "/usr/local/stow/erlang-r14b02/lib/erlang/bin" exec-path))
(require 'erlang-start)
;; Erlang Emacs Mode -- Configuration End

(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; (add-to-list 'load-path "~/.emacs.d/")
;; (autoload 'promela-mode "promela-mode" "PROMELA mode" nil t)
;; (setq auto-mode-alist
;;       (append
;;        (list (cons "\\.promela$"  'promela-mode)
;; 	     (cons "\\.spin$"     'promela-mode)
;; 	     (cons "\\.pml$"      'promela-mode)
;; 	     ;; (cons "\\.other-extensions$"     'promela-mode)
;;        	     )
;;        auto-mode-alist))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; (load "pandoc-mode")
;; (add-hook 'markdown-mode-hook 'turn-on-pandoc)
;; (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
;; (load "cm-mode")

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 113 :width normal)))))
