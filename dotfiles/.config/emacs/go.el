;;; go.el --- go specific  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-ts-mode
  :ensure nil
  :after eglot
  :mode ("\\.go\\'" "/go\\.mod\\'")
  :hook (go-ts-mode . (lambda ()
                        (setq compile-command "go build -v && go test -race -cover -v && go vet"
                              compilation-read-command nil
                              tab-width 2)
                        (eglot-ensure)
                        (add-hook 'before-save-hook #'eglot-format nil t)))
  :init
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . (eglot-gopls "gopls")))

  (defclass eglot-gopls (eglot-lsp-server) ()
    :documentation "A custom class for gopls.")

  (cl-defmethod eglot-initialization-options ((server eglot-gopls))
    "Passes through default gopls initialization options"
    '(:buildFlags ["-tags=integration,e2e"] :usePlaceholders t :staticcheck t)))

;;; go.el ends here
