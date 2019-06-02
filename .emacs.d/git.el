;;; git.el --- git specific

;;; Commentary:

;;; Code:

(use-package diff-hl
  :ensure t
  :demand
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (diff-hl . diff-hl-flydiff-mode))
  :config
  (global-diff-hl-mode)
  (unless window-system
    (diff-hl-margin-mode)))

(use-package magit
  :ensure t
  :after diff-hl
  :bind (("C-x g" . magit-status))
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (add-to-list 'display-buffer-alist
               `("magit:.*"
                 (display-buffer-in-side-window)
                 (slot         . 0)
                 (side         . right)
                 (window-width . 0.35)))
  (add-to-list 'display-buffer-alist
               `("magit-diff:.*"
                 (display-buffer-in-side-window)
                 (slot          . 1)
                 (side          . right)
                 (window-width  . 0.35)
                 (window-height . 0.3)))
  (add-to-list 'display-buffer-alist
               `("magit-process:.*"
                 (display-buffer-in-side-window)
                 (slot          . 2)
                 (side          . right)
                 (window-width  . 0.35)
                 (window-height . 0.3)))
  :config (setq magit-process-popup-time 0))

(use-package forge
  :ensure t
  :after magit)

;; don't use until there is a way to ignore folders...
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode))

;;; git.el ends here
