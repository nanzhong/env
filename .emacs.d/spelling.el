;;; spelling.el --- spelling and spell check specific

;;; Commentary:

;;; Code:

(use-package flyspell
  :ensure t
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;; spelling.el ends here
