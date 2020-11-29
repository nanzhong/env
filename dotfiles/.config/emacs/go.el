;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :after (lsp-mode)
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
                     (setq-local compile-command "go build -v; and go test -race -cover -v; and go vet")
                     (setq-local compilation-read-command nil)
                     (setq-local tab-width 2)
                     (setq-local lsp-gopls-build-flags ["-tags=integration"])
                     (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                     (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
  :bind (:map go-mode-map
              ("C-c C-c" . compile)))

(use-package go-projectile
  :ensure t
  :after (go-mode projectile))

;;; go.el ends here
