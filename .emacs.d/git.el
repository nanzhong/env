;;; git.el --- git specific

(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :bind
  (("C-x g" . magit-status)))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter-fr:side 'right-fringe))
