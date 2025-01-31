;;; misc-modes.el --- miscellaneous modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package rainbow-mode)

(use-package haml-mode)

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package markdown-mode
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))

(use-package fish-mode)

(use-package apib-mode
  :mode "\\.apib\\'")

(use-package lua-mode)

(use-package protobuf-mode)

(use-package nix-mode
  :demand t
  :mode "\\.nix\\'")

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ("Dockerfile\\'" "\\.Dockerfile\\'" "Dockerfile\\."))

(use-package cue-mode
  :ensure (:host github :repo "russell/cue-mode")
  :mode "\\.cue\\'")

;;; misc-modes.el ends here
