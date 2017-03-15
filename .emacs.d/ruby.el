;;; ruby.el --- ruby specific
(use-package robe
  :ensure t
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package enh-ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
  (setq enh-ruby-deep-indent-paren nil))

(use-package projectile-rails
  :ensure t
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package rinari
  :ensure t)

(use-package rspec-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rbenv
  :ensure t
  :config
  (global-rbenv-mode))
