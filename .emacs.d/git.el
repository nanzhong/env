;;; git.el --- git specific

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter-fr:side 'right-fringe))
