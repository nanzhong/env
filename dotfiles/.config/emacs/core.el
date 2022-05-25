;;; core.el --- core configuration

;;; Commentary:

;;; Code:

;; Startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice (lambda ()
    (org-roam-dailies-find-today)
    (get-buffer (format-time-string "%Y-%m-%d.org"))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Delete selection on typing
(delete-selection-mode 1)

;; Tab width
(setq tab-width 2)

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Disable bidirectional editing
(setq-default bidi-display-reordering nil)

;; Keep tmp files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Reduce scroll lag
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

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

(use-package calendar
  :mode ("\\(.+\\.\\)?diary\\'" . diary-mode)
  :config
  ;; Use ISO calendar date style
  (calendar-set-date-style 'iso)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(use-package comp
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package direnv
  :straight t
  :demand t
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

(use-package exec-path-from-shell
  :straight t
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package minions
  :straight t
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "⍠"
        minions-mode-line-delimiters '("" . "")))

(use-package flymake
  :straight t)


(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package vertico
  :straight t
  :bind (:map vertico-map
              ("C-M-j" . vertico-exit-input)
              ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package orderless
  :straight t
  :after vertico
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :after vertico
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key '(:debounce 0.5 any)))

(use-package embark
  :straight t
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
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package popon
  :straight (popon :type git :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :straight (corfu-terminal :type git :host nil :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after (popon))

(use-package corfu
  :straight t
  :after (corfu-popup)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (unless (display-graphic-p)
    (corfu-popup-mode +1)))

(use-package ace-window
  :straight t
  :bind (("C-x q" . ace-window))
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

(use-package dtrt-indent
  :straight t
  :config (dtrt-indent-global-mode))

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package clipetty
  :straight t
  :bind ("M-w" . clipetty-kill-ring-save))

(use-package tree-sitter
  :straight t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t)

;;; core.el ends here
