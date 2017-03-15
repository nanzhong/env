;;; git.el --- git specific

;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :bind
  (("C-x g" . magit-status)))

;; currently disabled as it makes magit even slower :(
;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode +1))

;;; git.el ends here
