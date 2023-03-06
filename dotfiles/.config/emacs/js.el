;;; js.el --- javascript related config

;;; Commentary:

;;; Code:

(use-package json-mode
  :demand t)

(use-package js2-mode
  :demand t
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t))

;;; js.el ends here
