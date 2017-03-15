;;; go.el --- go specific

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq gofmt-command "goimports")
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq tab-width 2))))

(use-package go-projectile
  :ensure t)

(use-package company-go
  :ensure t
  :config
  (eval-after-load 'company
    '(push 'company-go company-backends))
  (setq company-go-show-annotation t))
