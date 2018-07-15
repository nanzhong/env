;;; misc-modes.el --- miscellaneous modes

;;; Commentary:

;;; Code:

(use-package rainbow-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package fish-mode
  :ensure t)

(use-package apib-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode)))

(use-package lua-mode
  :ensure t)

(use-package company-lua
  :ensure t
  :after company)

(use-package protobuf-mode
  :ensure t)

;;; misc-modes.el ends here
