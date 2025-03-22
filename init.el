;;; TODO: Install an lsp for python
;;; TODO: dired-find-alternate-file



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

(use-package company
  :ensure t
  :bind
  ( :map company-mode-map
    ("M-s /" . company-complete))
  :config
  (setq company-idle-delay 100)
  )

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package lsp-mode
  :ensure t
  :config
  (require 'dap-cpptools)
  )

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable nil)
  )

(use-package dap-mode
  :ensure t
  :preface
  (defvar dap-mode-was-just-installed (not (package-installed-p 'dap-mode)))
  :config
  (if dap-mode-was-just-installed (dap-cpptools-setup))
  )

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
           :chat-model "phi4"
           :embedding-model "nomic-embed-text"))
  )

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(defun open-init-file ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun project-find-file-at-point ()
  "Search for a file in the current project matching the filename at point."
  (interactive)
  (let* ((project (project-current))
         (root (when project (project-root project)))
         (filename (substring-no-properties (thing-at-point 'filename)))
         (files (when project (project-files project)))
         (matches (when (and filename (not (string-empty-p filename)))
                    (seq-filter (lambda (f) (string-match-p (regexp-quote filename) f)) files))))
    (cond
     ((not project) (message "Not in a project."))
     ((not filename) (message "No filename found at point."))
     ((null matches) (message "No matching file found for: %s" filename))
     ((= (length matches) 1) (find-file (expand-file-name (car matches) root)))
     (t (find-file (expand-file-name (completing-read "Select file: " matches) root))))))

(defun copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(define-key global-map (kbd "M-s c") #'open-init-file)
(define-key global-map (kbd "C-x b") #'counsel-switch-buffer)
(define-key global-map (kbd "M-s s") #'counsel-at-point-rg)
(define-key global-map (kbd "M-s f") #'project-find-file-at-point)
(define-key global-map (kbd "M-s w") #'copy-symbol-at-point)
(define-key global-map (kbd "M-s S") #'counsel-rg)
(define-key global-map (kbd "M-s d") #'lsp-ui-doc-show)

(defun my-c++-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (lsp)
  (lsp-ui-mode)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package which-key)
(setq which-key-idle-delay 0.4)
(which-key-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

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
 '(fringe-mode '(1 . 1) nil (fringe))
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(ace-window achievements all-the-icons anzu auctex blamer
                clean-aindent-mode cmake-mode company
                company-c-headers compat consult counsel-at-point
                dap-mode dired-subtree doom-modeline dtrt-indent
                ellama evil git god-mode golden-ratio google-this
                gptel lsp-ui magit orderless prescient projectile
                pyvenv ripgrep solaire-mode spacemacs-theme
                tree-sitter tree-sitter-langs undo-tree use-package
                vertico vertico-prescient which-key zygospore))
 '(truncate-lines t)
 '(window-divider-default-right-width 18))


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
(doom-modeline-mode 1)
(achievements-mode +1)

(add-to-list 'company-backends 'company-c-headers)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "M-o") 'ace-window)

(setq project-vc-ignores '("build/" ".clangd/" ".git/" "compile_commands.json"))

(setq doom-modeline-icon nil)

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(setq buffer-age-limit 1800)
(setq inhibit-startup-screen t)
(setq ring-bell-function (lambda (&rest _) )) ; Disable the stupid sound effect on error.

(set-face-foreground 'vertical-border (if (true-color-p) "#292b2e" "#262626"))
(set-face-attribute  'mode-line          nil :box nil)
(set-face-attribute  'mode-line-inactive nil :box nil)
(set-face-background 'mode-line          "gray9")
(set-face-background 'mode-line-inactive "gray9")
(set-face-background 'doom-modeline-bar  "white")
(set-face-background 'doom-modeline-bar-inactive "gray9")
(set-face-background 'line-number        "grey9")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)

(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))
(defun dired-left ()
  "Display default directory on left"
  (interactive)
  (let ((buffer (dired-noselect "/home/justin")))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left)
              (slot  . 0)
              (window-width . 40)
              (preserve-size . (t. nil)), parameters))
    ))
(put 'dired-find-alternate-file 'disabled nil)
