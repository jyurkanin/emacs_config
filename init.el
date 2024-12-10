(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     t)
(setq package-check-signature nil) ; This disables checking the package manager signature.

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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
)

(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
)

(use-package prescient
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  :catch (lambda (keyword err) (message (error-message-string err)))
  )


(use-package vertico-prescient
  :init
  (vertico-prescient-mode)
  :catch (lambda (keyword err) (message (error-message-string err)))
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package golden-ratio)
(use-package all-the-icons :if (display-graphic-p))

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setq ellama-language "English")
  (require 'llm-ollama)
  (setq ellama-provider
          (make-llm-ollama
           :chat-model "llama3";"llama3:8b-instruct-q8_0"
           :embedding-model "nomic-embed-text"))
  )

(defun my-c++-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package which-key)
(setq which-key-idle-delay 0.4)
(which-key-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

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
 '(custom-safe-themes t)
 '(electric-pair-mode nil)
 '(electric-pair-open-newline-between-pairs nil)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(ace-window achievements all-the-icons anzu auctex blamer
                clean-aindent-mode cmake-mode company
                company-c-headers compat dap-mode doom-modeline
                dtrt-indent ellama git golden-ratio magit orderless
                prescient solaire-mode spacemacs-theme tree-sitter
                tree-sitter-langs undo-tree use-package vertico
                vertico-prescient which-key zygospore))
 '(truncate-lines t))


(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(global-eldoc-mode -1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-anzu-mode t)
(clean-aindent-mode t)
(global-company-mode t)
(global-auto-revert-mode t)
(dtrt-indent-global-mode t)
;; (solaire-global-mode +1)
;; (golden-ratio-mode 1)
(doom-modeline-mode 1)
(achievements-mode +1)

(add-to-list 'company-backends 'company-c-headers)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "M-o") 'ace-window)

(setq project-vc-ignores '("build/" ".clangd/" ".git/" "compile_commands.json"))

(setq doom-modeline-icon nil)
(set-frame-font "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-9" nil t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
