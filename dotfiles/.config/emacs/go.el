;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :after (lsp-mode)
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local compile-command "go build -v; and go test -v; and go vet")
                            (setq-local compilation-read-command nil)
                            (setq-local tab-width 2)
                            (add-to-list 'display-buffer-alist
                                         `("\\*compilation\\*"
                                           (display-buffer-reuse-window
                                            display-buffer-in-side-window)
                                           (slot          . 0)
                                           (side          . bottom)
                                           (window-height . 0.35)))
                            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                            (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

(use-package go-projectile
  :ensure t
  :after (go-mode projectile))

;;; go.el ends here
