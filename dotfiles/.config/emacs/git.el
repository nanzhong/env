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

(use-package git-modes
  :straight t)

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq magit-process-popup-time 0
        magit-bury-buffer-function 'magit-mode-quit-window
        magit-display-buffer-function #'display-buffer)
  (add-to-list 'display-buffer-alist
               `(,(make-display-buffer-matcher-function '(magit-mode))
                 (display-buffer-reuse-mode-window display-buffer-in-side-window)
                 (mode magit-mode)
                 (window-width . 100)
                 (dedicated . t)
                 (side . left)
                 (slot . 0)))
  (add-to-list 'display-buffer-alist
               '(".*COMMIT_EDITMSG"
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . -1))))

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
