;;; git.el --- git specific

;;; Commentary:

;;; Code:

(use-package diff-hl
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (diff-hl . diff-hl-flydiff-mode))
  :config
  (global-diff-hl-mode)
  (unless window-system
    (diff-hl-margin-mode)))

(use-package git-modes
  :demand t)

(use-package magit
  :elpaca (magit :host github :repo "magit/magit")
  :demand t
  :bind (("C-x g" . magit-status))
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq magit-process-popup-time 0
        magit-bury-buffer-function #'magit-restore-window-configuration
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; (use-package forge
;;   :demand t
;;   :after magit
;;   :config
;;   (setq forge-database-file "~/.cache/forge-database.sqlite")
;;   (push '("github.internal.digitalocean.com" "github.internal.digitalocean.com/api/v3"
;;           "github.internal.digitalocean.com" forge-github-repository)
;;         forge-alist))

(use-package hl-todo
  :elpaca (hl-todo :host github :repo "tarsius/hl-todo"))

(use-package magit-todos
  :after magit hl-todo
  :config (magit-todos-mode))

(use-package git-link
  :elpaca (git-link :host github :repo "sshaw/git-link")
  :bind (("C-c g l" . git-link)))
;;; git.el ends here
