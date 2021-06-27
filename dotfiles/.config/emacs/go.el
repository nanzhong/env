;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :straight t
  :after (lsp-mode)
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
                     (setq-local compile-command "go build -v && go test -race -cover -v && go vet")
                     (setq-local compilation-read-command nil)
                     (setq-local tab-width 2)
                     (setq-local lsp-gopls-build-flags ["-tags=integration,sandbox"])
                     (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")
                     (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                     (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
  :bind (:map go-mode-map
              ("C-c C-c" . compile)))

(use-package go-projectile
  :straight t
  :after (go-mode projectile))

;;; go.el ends here
