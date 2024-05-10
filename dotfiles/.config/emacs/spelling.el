;;; spelling.el --- spelling and spell check specific

;;; Commentary:

;;; Code:

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; spelling.el ends here
