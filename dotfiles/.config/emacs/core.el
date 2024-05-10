;;; -*- lexical-binding: t; -*-
;;; core.el --- core configuration

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  ;; Startup
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)

  ;; Make backups of files, even when they're in version control
  (setq vc-make-backup-files t)

  ;; Delete selection on typing
  (delete-selection-mode 1)

  ;; Tab width
  (setq tab-width 2)

  ;; Prevent Extraneous Tabs
  (setq-default indent-tabs-mode nil)

  ;; Keep tmp files out of the way
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; Disable lockfiles
  (setq create-lockfiles nil)

  ;; Don't litter buffer list with dired buffers
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Enable auto-revert-mode
  (global-auto-revert-mode)

  ;; Use ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; Minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)

  ;; Change default tab behaviour to trigger completion-at-point as well.
  (setq tab-always-indent 'complete)

  ;; Manual buffer switching should also obey display action rules.
  (setq switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window 'pop)
  (defun make-display-buffer-matcher-function (major-modes)
    (lambda (buffer-name action)
      (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))
  (setq display-buffer-alist
        `(("\\*compilation\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-width . 80)
           (dedicated . t)
           (side . right)
           (slot . 0))
          ("\\*apropos\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-width . 80)
           (dedicated . t)
           (side . right)
           (slot . 0))
          ("\\*help\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-width . 80)
           (dedicated . t)
           (side . right)
           (slot . 1))))

  (defconst prog-like-modes
    '(prog-mode org-mode nix-mode conf-mode yaml-mode)
    "Major modes that are programming like."))

(use-package calendar
  :ensure nil
  :mode ("\\(.+\\.\\)?diary\\'" . diary-mode)
  :config
  ;; Use ISO calendar date style
  (calendar-set-date-style 'iso)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(use-package comp
  :ensure nil
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package direnv
  :demand t
  :config
  (direnv-mode))

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode
              ("C-c e" . flymake-show-buffer-diagnostics))
  :config (setq flymake-suppress-zero-counters nil
                flymake-mode-line-lighter ""
                flymake-mode-line-counter-format `("["
                                                   ,(propertize "⨂ " 'face 'compilation-error) (:eval (flymake--mode-line-counter :error t))
                                                   ,(propertize " ⨁ " 'face 'compilation-warning) (:eval (flymake--mode-line-counter :warning t))
                                                   ,(propertize " ⨀ " 'face 'compilation-info) (:eval (flymake--mode-line-counter :note t))
                                                   "]")))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :demand t
  :after yasnippet)

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode t))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-M-j" . vertico-exit-input)
              ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package orderless
  :demand t
  :after vertico
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :demand t
  :after vertico
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.1
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-preview-key '(:debounce 0.5 any)))

(use-package embark
  :demand t
  :bind (:map minibuffer-local-map
              ("M-." . embark-act)
              ("M-," . embark-dwim)
              ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand t
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :ensure (corfu-terminal :host codeberg :repo "akib/emacs-corfu-terminal")
  :demand t
  :hook (after-make-frame-functions . (lambda ()
                                        (unless (display-graphic-p)
                                          (corfu-terminal-mode +1)))))

(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package ace-window
  :demand t
  :bind (("C-x q" . ace-window))
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package editorconfig
  :demand t
  :config (editorconfig-mode 1))

(use-package dtrt-indent
  :demand t
  :config (dtrt-indent-global-mode))

(use-package which-key
  :demand t
  :config (which-key-mode))

(use-package clipetty
  :demand t
  :bind (:map universal-argument-map
              ("M-w" . clipetty-kill-ring-save))
  :config
  (setq clipetty-tmux-ssh-tty "echo \"SSH_TTY=$(tmux display-message -p '#{pane_tty}')\""))

;;; core.el ends here
