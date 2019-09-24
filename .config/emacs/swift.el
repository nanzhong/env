;; swift.el --- swift specific

(use-package swift-mode
  :ensure t)

(use-package company-sourcekit
  :ensure t
  :after (company swift-mode)
  :hook (swift-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             (cons 'company-sourcekit company-backends))))
  :config
  (setq sourcekit-sourcekittendaemon-executable "/Users/nan/src/SourceKittenDaemon/.build/debug/sourcekittend"))

;; (use-package flycheck-swift
;;   :ensure t
;;   :config
;;   (eval-after-load 'flycheck'
;;     '(flycheck-swift-setup)))
