(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package git)

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

; (use-package mo-vi-ment-mode)


(windmove-default-keybindings) ; This is for shift-arrow key movement between windows

(defun my-c++-mode-hook ()
  (eglot-ensure)
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  )

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package which-key)
(setq which-key-idle-delay 0.4)
(which-key-mode)


(load-file "~/.emacs.d/my_move_mode.el")


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-by-copying t)
 '(backup-directory-alist '(("" . "~/.emacs.d/backup")))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(electric-pair-mode t)
 '(electric-pair-open-newline-between-pairs nil)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(my_move_mode mo-vi-ment-mode which-key magit blamer git auctex projectile eglot spacemacs-theme company-c-headers tree-sitter-langs tree-sitter zygospore use-package undo-tree dtrt-indent company comment-dwim-2 clean-aindent-mode anzu))
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
