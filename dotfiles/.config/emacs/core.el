;;; core.el --- core configuration

;;; Commentary:

;;; Code:

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

;; Configure cpcat
(global-set-key (kbd "M-W") (lambda ()
                              (interactive)
                              (shell-command-on-region (region-beginning) (region-end) "cpcat > (tmux run-shell 'echo #{pane_tty}')")))

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package direnv
  :ensure t
  :demand t
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 3
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package flymake
  :ensure t)


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package company
  :ensure t
  :config
  (setq-default company-idle-delay 0.25
                company-echo-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-downcase nil)
  (global-company-mode))

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs tab-mark indentation lines-tail))
  (global-whitespace-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package flx
  :ensure t
  :demand t)

(add-to-list 'package-pinned-packages '(swiper . "melpa"))
(add-to-list 'package-pinned-packages '(ivy . "melpa"))
(use-package ivy
  :ensure t
  :demand t
  :hook (eshell-mode
         . (lambda ()
             (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
             ;; only if you want to use the minibuffer for completions instead of the
             ;; in-buffer interface
             (setq-local ivy-display-functions-alist
                         (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                               ivy-display-functions-alist))))
  :bind (("C-s" . search-forward)
         ("M-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

(use-package projectile
  :ensure t
  :after (flx ivy)
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :after (ivy projectile)
  :config
  (counsel-projectile-mode))

(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :hook ((ibuffer-mode . (lambda ()
                           (ibuffer-projectile-set-filter-groups)
                           (unless (eq ibuffer-sorting-mode 'alphabetic)
                             (ibuffer-do-sort-by-alphabetic))))))

(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-char)
         ("C-," . avy-goto-char-2)))

(use-package ace-window
  :ensure t
  :bind (("M-RET" . ace-window))
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; core.el ends here
