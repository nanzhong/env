;;; eglot.el --- eglot configuration

;;; Commentary:

;;; Code:
(use-package eglot
  :elpaca nil
  :demand t
  :bind (:map eglot-mode-map
              ("C-l r" . eglot-rename)
              ("C-l f" . eglot-format)
              ("C-l a" . eglot-code-actions))
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

;;; eglot.el ends here
