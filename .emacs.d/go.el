;;; go.el --- go specific

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq tab-width 2))))

(use-package company-go
  :ensure t
  :config
  (eval-after-load 'company
    '(push 'company-go company-backends))
  (setq company-go-show-annotation t))
