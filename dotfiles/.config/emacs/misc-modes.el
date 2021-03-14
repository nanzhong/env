;;; misc-modes.el --- miscellaneous modes

;;; Commentary:

;;; Code:

(use-package rainbow-mode
  :straight t)

(use-package haml-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(use-package markdown-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package fish-mode
  :straight t)

(use-package apib-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode)))

(use-package lua-mode
  :straight t)

(use-package company-lua
  :straight t
  :after company)

(use-package protobuf-mode
  :straight t)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

;;; misc-modes.el ends here
