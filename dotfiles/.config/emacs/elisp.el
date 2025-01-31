;;; elisp.el. --- elisp config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)))

;;; elisp.el ends here
