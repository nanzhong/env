;;; git.el --- git specific

;;; Commentary:

;;; Code:

(use-package diff-hl
  :straight t
  :demand
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (diff-hl . diff-hl-flydiff-mode))
  :config
  (global-diff-hl-mode)
  (unless window-system
    (diff-hl-margin-mode)))

(use-package magit
  :straight t
  :after diff-hl
  :bind (("C-x g" . magit-status))
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (setq magit-process-popup-time 0
                magit-bury-buffer-function 'magit-mode-quit-window))

;; (use-package forge
;;   :straight t
;;   :after magit
;;   :config
;;   (setq forge-database-file "~/.cache/forge-database.sqlite")
;;   (push '("github.internal.digitalocean.com" "github.internal.digitalocean.com/api/v3"
;;           "github.internal.digitalocean.com" forge-github-repository)
;;         forge-alist))

;; don't use until there is a way to ignore folders...
;; (use-package magit-todos
;;   :straight t
;;   :after magit
;;   :config (magit-todos-mode))

;;; git.el ends here
