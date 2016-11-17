;;; core.el --- core configuration

;; Emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; Startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Delete selection on typing
(delete-selection-mode 1)

;; Tab width
(setq tab-width 2)

;; Disable bidirectional editing
(setq-default bidi-display-reordering nil)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0.1))

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens-config
  :ensure smartparens
  :config
  (smartparens-global-mode t))

(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)))

(use-package projectile
  :ensure t
  :config
  ;; this hack is needed because tramp hangs otherwise
  ; (projectile-global-mode)
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t)

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package shackle
  :ensure t
  :config
  (setq helm-display-function 'pop-to-buffer)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
			("\\`\\*magit.*\\'" :regexp t :align right :size 0.4)))
  (shackle-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-window)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))
