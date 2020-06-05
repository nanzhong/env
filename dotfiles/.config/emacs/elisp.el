;;; elisp.el. --- elisp config
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq-default indent-tabs-mode nil)))

;;; elisp.el ends here
