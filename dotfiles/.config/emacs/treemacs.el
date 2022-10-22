;;; treemacs.el --- treemacs configuration

;;; Commentary:

;;; Code:

(use-package treemacs
  :straight t
  :defer t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :straight t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

;;; treemacs.el ends here
