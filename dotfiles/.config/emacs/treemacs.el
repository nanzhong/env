;;; treemacs.el --- treemacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package treemacs
  :demand t
  :defer t
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-all-the-icons
  :after treemacs all-the-icons
  :demand t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :demand t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :demand t)

;;; treemacs.el ends here
