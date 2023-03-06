;;; ruby.el --- ruby specific

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :elpaca (ruby-mode :host github :repo "ruby/elisp")
  :demand t)

(use-package inf-ruby
  :demand t
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package rspec-mode
  :demand t
  :after inf-ruby
  :hook (after-init-hook . inf-ruby-switch-setup))

;;; ruby.el ends here
