;;; appearance.el --- theme and ui related config

;;; Commentary:

;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-appearance . light))
(setq frame-title-format nil)

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))

;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/src/emacs-nan-theme")
(load-theme 'nan t)

(fringe-mode 5)

;; No bell
(setq visible-bell t
      ring-bell-function 'ignore)

;; Maximum styles
(setq font-lock-maximum-decoration t)

;; Bar cursor
(setq-default cursor-type 'bar)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show line number
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; Show trailing whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))

;; Indent guides
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'column))

;; Modeline
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (setq moody-slant-function #'moody-slant-apple-rgb)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; appearance.el ends here
