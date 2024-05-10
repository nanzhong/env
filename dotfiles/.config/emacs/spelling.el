;;; spelling.el --- spelling and spell check specific

;;; Commentary:

;;; Code:

(use-package jinx
  :ensure nil
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;; spelling.el ends here
