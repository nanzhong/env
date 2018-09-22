;;; spelling.el --- spelling and spell check specific

;;; Commentary:

;;; Code:

(use-package flyspell
  :ensure t
  :hook ((text-mode . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         (prog-mode . flyspell-mode)))

;;; spelling.el ends here
