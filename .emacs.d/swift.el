;; swift.el --- swift specific

(use-package swift-mode
  :ensure t)

(use-package company-sourcekit
  :ensure t
  :config
  (setq sourcekit-sourcekittendaemon-executable "/Users/nan/src/SourceKittenDaemon/.build/debug/sourcekittend")
  (eval-after-load 'company
    '(push 'company-sourcekit company-backends)))

(use-package flycheck-swift
  :ensure t
  :config
  (eval-after-load 'flycheck'
    '(flycheck-swift-setup)))
