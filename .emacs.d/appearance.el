;;; appearance.el --- theme and ui related config
(setq default-frame-alist '((font . "Menlo 11")))
;;(set-face-bold-p 'bold nil)

;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/nine27-theme")

(load-theme 'nine27 t)

;; No bell
(setq visible-bell t
      ring-bell-function 'ignore)

;; Maximum styles
(setq font-lock-maximum-decoration t)

;; Bar cursor
(setq-default cursor-type 'bar)

;; Highlight current line
;;(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show line number
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
(setq linum-format " %4d ")

;; Show trailing whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'slant
	powerline-height 20))
