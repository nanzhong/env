;;; lsp.el --- lsp configuration

;;; Commentary:

;;; Code:
(use-package lsp-mode
  :ensure t
  :after direnv
  :commands lsp
  :hook (prog-mode . (lambda ()
                       (direnv-update-environment)
                       (lsp)))
  :config
  (setq lsp-enable-indentation t
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting t
        lsp-idle-delay 1.0
        lsp-keep-workspace-alive nil
        lsp-lens-debounce-interval 1.0))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-doc-use-childframe nil))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list)

;;; lsp.el ends here
