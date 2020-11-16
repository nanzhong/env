;;; js.el --- javascript related config

;;; Commentary:

;;; Code:

(use-package json-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t))

(use-package vue-mode
  :ensure t
  :config
  (setq mmm-submode-decoration-level 0))

;; (use-package nvm
;;   :ensure t)

;;; js.el ends here
