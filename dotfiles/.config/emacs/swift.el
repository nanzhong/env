;; swift.el --- swift specific

(use-package swift-mode
  :straight t)

(use-package company-sourcekit
  :straight t
  :after (company swift-mode)
  :hook (swift-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             (cons 'company-sourcekit company-backends))))
  :config
  (setq sourcekit-sourcekittendaemon-executable "/Users/nan/src/SourceKittenDaemon/.build/debug/sourcekittend"))
