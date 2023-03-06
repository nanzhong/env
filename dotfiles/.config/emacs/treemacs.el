;;; treemacs.el --- treemacs configuration

;;; Commentary:

;;; Code:

(use-package treemacs
  :demand t
  :defer t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :demand t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :demand t)

;;; treemacs.el ends here
