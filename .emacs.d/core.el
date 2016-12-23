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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

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

(use-package flx
  :ensure t)

(add-to-list 'package-pinned-packages '(swiper . "melpa"))
(add-to-list 'package-pinned-packages '(ivy . "melpa"))
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;;(global-set-key (kbd "<f1> l") 'counsel-load-library)
  ;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  ;;(global-set-key (kbd "C-x l") 'counsel-locate)
  ;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

(use-package projectile
  :ensure t
  :config
  ;; this hack is needed because tramp hangs otherwise
  ; (projectile-global-mode)
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

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

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-.") 'avy-goto-char)
  (global-set-key (kbd "C-,") 'avy-goto-char-2))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-<return>") 'ace-window)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))
