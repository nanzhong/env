;;; treemacs.el --- treemacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package treemacs
  :demand t)

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

(use-package treemacs-icons-dired
  :after treemacs)

(use-package treemacs-magit
  :after treemacs magit)

;;; treemacs.el ends here
