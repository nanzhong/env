;;; appearance.el --- theme and ui related config

;;; Commentary:

;;; Code:

(setq default-frame-alist '((font . "Menlo 11")))
;;(set-face-bold-p 'bold nil)

;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package all-the-icons
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/nan-theme")
(load-theme 'nan t)
(nan-setup-modeline-format)
;; (use-package zerodark-theme
;;   :ensure t
;;   :config
;;   (zerodark-setup-modeline-format))

(fringe-mode 15)

;; No bell
(setq visible-bell t
      ring-bell-function 'ignore)

;; Maximum styles
(setq font-lock-maximum-decoration t)

;; Bar cursor
(setq-default cursor-type 'bar)

;; Highlight current line
;;(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show line number
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
(setq linum-format " %4d")

;; Show trailing whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))

;;; appearance.el ends here
