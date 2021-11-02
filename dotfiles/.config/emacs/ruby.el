;;; ruby.el --- ruby specific
(use-package enh-ruby-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
  (setq enh-ruby-deep-indent-paren nil))

(use-package inf-ruby
  :straight t
  :after enh-ruby-mode
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package rspec-mode
  :straight t
  :after inf-ruby
  :hook (after-init-hook . inf-ruby-switch-setup))

(use-package rinari
  :straight t)
