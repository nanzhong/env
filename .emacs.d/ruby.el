;;; ruby.el --- ruby specific
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

(use-package robe
  :ensure t
  :after (enh-ruby-mode company)
  :hook ((enh-ruby-mode-hook . robe-mode)
         (robe-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             (cons 'company-robe company-backends))))))

(use-package inf-ruby
  :ensure t
  :after enh-ruby-mode
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package rspec-mode
  :ensure t
  :after inf-ruby
  :hook (after-init-hook . inf-ruby-switch-setup))

(use-package projectile-rails
  :ensure t
  :after projectile
  :hook (projectile-mode . projectile-rails-on))

(use-package rinari
  :ensure t)
