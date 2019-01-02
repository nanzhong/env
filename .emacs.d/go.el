;;; go.el --- go specific

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :config
  (setq gofmt-command "goimports")
  (setq gofmt-args '("-local" "do"))
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local compile-command "go build -v; and go test -v; and go vet")
                            (setq-local compilation-read-command nil)
                            (setq-local tab-width 2)))
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-projectile
  :ensure t
  :after (go-mode projectile))

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
