;;; js.el --- javascript related config

;;; Commentary:

;;; Code:

(use-package json-mode
  :straight t)

(use-package js2-mode
  :straight t
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t))

(use-package vue-mode
  :straight t
  :config
  (setq mmm-submode-decoration-level 0))

;;; js.el ends here
