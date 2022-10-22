;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
                     (setq compile-command "go build -v && go test -race -cover -v && go vet")
                     (setq compilation-read-command nil)
                     (setq tab-width 2)
                     (add-hook 'before-save-hook #'eglot-format nil t)))
  :bind (:map go-mode-map
              ("C-c C-c" . compile)))

;;; go.el ends here
