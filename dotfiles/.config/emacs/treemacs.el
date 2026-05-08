;;; treemacs.el --- treemacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package treemacs
  :demand t
  :bind (("C-c t t" . treemacs))
  :config
  ;; Replicate `treemacs-follow-mode' semantics, but update explicitly on
  ;; window/buffer focus changes rather than on an idle timer driven by
  ;; `buffer-list-update-hook'. `treemacs--follow' is a no-op unless a
  ;; treemacs window is visible in the current scope and the target file
  ;; lives under a workspace project, so it's safe to run on every change.
  (defun my/treemacs-follow (&rest _)
    "Update the treemacs buffer to focus the currently selected file."
    (treemacs--follow))
  (add-hook 'window-buffer-change-functions #'my/treemacs-follow)
  (add-hook 'window-selection-change-functions #'my/treemacs-follow)

  (treemacs-fringe-indicator-mode 'always)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

(use-package treemacs-icons-dired
  :after treemacs)

(use-package treemacs-magit
  :after treemacs magit)

;;; treemacs.el ends here
