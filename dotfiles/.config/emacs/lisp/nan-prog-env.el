;;; nan-prog-env.el --- Customizations for prog like modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Nan Zhong

;; Author: Nan Zhong <me@nanzhong.com>
;; Version: 0.0.1
;; Package-Requires: (whitespace highlight-indent-guides)
;; Keywords: mode-line

;;; Commentary:

;;; Code:

(require 'whitespace)
(require 'highlight-indent-guides)

(defun setup-prog-env ()
  "Setup prog like environment."
  (display-line-numbers-mode)
  ;; From highlight-indent-guides
  (highlight-indent-guides-mode)
  ;; From whitespace
  (whitespace-mode)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t))

(require 'derived)
(dolist (hook (mapcar #'derived-mode-hook-name prog-like-modes))
  (add-hook hook 'setup-prog-env))

(provide 'nan-prog-env)

;;; nan-prog-env.el ends here
