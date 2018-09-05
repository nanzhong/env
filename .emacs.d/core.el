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

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'global-flycheck-mode))

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
  (setq-default company-idle-delay 0.1
                company-echo-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-downcase nil
                company-backends '((company-capf company-dabbrev-code company-etags company-keywords company-yasnippet company-files)
                                   (company-dabbrev company-abbrev company-ispell)))
  (global-company-mode))

;; (use-package company-quickhelp
;;   :ensure t
;;   :after company
;;   :config
;;   (setq company-quickhelp-delay 1)
;;   (company-quickhelp-mode))

;; (use-package company-box
;;   :ensure t
;;   :after company
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-align-annotations t)
;;   (setq company-box-icons-unknown 'fa_question_circle)
;;   (setq company-box-icons-elisp
;;         '((fa_tag :face font-lock-function-name-face) ;; Function
;;           (fa_cog :face font-lock-variable-name-face) ;; Variable
;;           (fa_cube :face font-lock-constant-face) ;; Feature
;;           (md_color_lens :face font-lock-doc-face))) ;; Face
;;   (setq company-box-icons-yasnippet 'fa_bookmark)
;;   (setq company-box-icons-lsp
;;         '((1 . fa_text_height) ;; Text
;;           (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
;;           (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
;;           (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
;;           (5 . (fa_cog :foreground "#FF9800")) ;; Field
;;           (6 . (fa_cog :foreground "#FF9800")) ;; Variable
;;           (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
;;           (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
;;           (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
;;           (10 . (fa_cog :foreground "#FF9800")) ;; Property
;;           (11 . md_settings_system_daydream) ;; Unit
;;           (12 . (fa_cog :foreground "#FF9800")) ;; Value
;;           (13 . (md_storage :face font-lock-type-face)) ;; Enum
;;           (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
;;           (15 . md_closed_caption) ;; Snippet
;;           (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
;;           (17 . fa_file_text_o) ;; File
;;           (18 . md_refresh) ;; Reference
;;           (19 . fa_folder_open) ;; Folder
;;           (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
;;           (21 . (fa_square :face font-lock-constant-face)) ;; Constant
;;           (22 . (fa_cube :face font-lock-type-face)) ;; Struct
;;           (23 . fa_calendar) ;; Event
;;           (24 . fa_square_o) ;; Operator
;;           (25 . fa_arrows)) ;; TypeParameter
;;         ))

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

(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'notifier))

(use-package flx
  :ensure t)

(add-to-list 'package-pinned-packages '(swiper . "melpa"))
(add-to-list 'package-pinned-packages '(ivy . "melpa"))
(use-package ivy
  :ensure t
  :hook (eshell-mode
         . (lambda ()
             (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
             ;; only if you want to use the minibuffer for completions instead of the
             ;; in-buffer interface
             (setq-local ivy-display-functions-alist
                         (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                               ivy-display-functions-alist))))
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (global-set-key (kbd "C-s") 'search-forward)
  (global-set-key (kbd "M-s") 'counsel-grep-or-swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;;(global-set-key (kbd "<f1> l") 'counsel-load-library)
  ;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c r") 'counsel-rg)
  ;;(global-set-key (kbd "C-x l") 'counsel-locate)
  ;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :after (ivy projectile)
  :config
  (counsel-projectile-mode))

(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(("magit" :regexp t :align right :size 0.4)))
  (shackle-mode))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-.") 'avy-goto-char)
  (global-set-key (kbd "C-,") 'avy-goto-char-2))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-RET") 'ace-window)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; core.el ends here
