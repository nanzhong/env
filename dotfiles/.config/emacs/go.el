;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :straight t
  :after eglot
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
                     (setq compile-command "go build -v && go test -race -cover -v && go vet"
                           compilation-read-command nil
                           tab-width 2)
                     (eglot-ensure)
                     (add-hook 'before-save-hook #'eglot-format nil t)))
  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :init
  (add-to-list 'eglot-server-programs
               '(go-mode . (eglot-gopls "gopls")))

  (defclass eglot-gopls (eglot-lsp-server) ()
    :documentation "A custom class for gopls.")

  (cl-defmethod eglot-initialization-options ((server eglot-gopls))
    "Passes through default gopls initialization options"
    '(:usePlaceholders t :staticcheck t)))

;;; go.el ends here
