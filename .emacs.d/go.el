;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :bind (("C-c C-c" . compile))
  :hook ((go-mode . (lambda ()
                      (setq gofmt-command "goimports")
                      (setq tab-width 2)
                      (setq compile-command "go build -v; and go test -v; and go vet")
                      (setq compilation-read-command nil)))
         (before-save . gofmt-before-save)))

(use-package go-projectile
  :ensure t
  :after (go-mode projectile))

(use-package lsp-go
  :ensure t
  :hook (go-mode . lsp-go-enable))

;; (use-package go-eldoc
;;   :ensure t
;;   :after go-mode
;;   :hook (go-mode . go-eldoc-setup))

;; (use-package company-go
;;   :ensure t
;;   :after (go-mode company)
;;   :hook (go-mode . (lambda ()
;;                      (set (make-local-variable 'company-backends)
;;                           (cons 'company-go company-backends))))
;;   :config (setq company-go-show-annotation t))

;;; go.el ends here
