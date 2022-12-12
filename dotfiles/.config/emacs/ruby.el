;;; ruby.el --- ruby specific

;;; Commentary:

;;; Code:

(use-package inf-ruby
  :straight t
  :after ruby-mode
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package rspec-mode
  :straight t
  :after inf-ruby
  :hook (after-init-hook . inf-ruby-switch-setup))

;;; ruby.el ends here
