;;; misc-modes.el --- miscellaneous modes

;;; Commentary:

;;; Code:

(use-package rainbow-mode
  :demand t)

(use-package haml-mode
  :demand t)

(use-package yaml-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(use-package markdown-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package fish-mode
  :demand t)

(use-package apib-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode)))

(use-package lua-mode
  :demand t)

(use-package protobuf-mode
  :demand t)

(use-package nix-mode
  :demand t
  :mode "\\.nix\\'")

;;; misc-modes.el ends here
