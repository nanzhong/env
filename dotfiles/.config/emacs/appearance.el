;;; appearance.el --- theme and ui related config

;;; Commentary:

;;; Code:

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'nan t)

(menu-bar-mode -1)
(if window-system
    (progn (fringe-mode 5)
           (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
           ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
           (add-to-list 'default-frame-alist '(ns-appearance . light))
           (setq frame-title-format nil)
           (add-to-list 'default-frame-alist '(font . "Iosevka 11"))
           (tool-bar-mode -1)
           (scroll-bar-mode -1))
  (progn (xterm-mouse-mode 1)
         (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
         (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

;; No bell
(setq visible-bell t
      ring-bell-function 'ignore)

;; Maximum styles
(setq font-lock-maximum-decoration t)

;; Allow fitting window width
(setq fit-window-to-buffer-horizontally t)

;; Bar cursor
(setq-default cursor-type 'bar)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show line number
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; Enable column mode
(column-number-mode 1)

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
(if window-system
  (use-package moody
    :ensure t
    :config
    (setq x-underline-at-descent-line t)
    (setq moody-mode-line-height 24)nnppnn
    (setq moody-slant-function #'moody-slant-apple-rgb)n∑
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode))
  (setq-default mode-line-format
                `(
                  "%e"
                  "⌈" mode-line-mule-info "⌋"
                  " "
                  "(" mode-line-modified mode-line-remote ")"
                  "    "
                  ,(propertized-buffer-identification "⌈%b⌋")
                  " "
                  ;;; (:propertize "⌈%l,%c⌋" face mode-line-position-face)
                  "⌈%l,%c⌋"
                  "    "
                  minions-mode-line-modes
                  "    "
                  (vc-mode vc-mode)
                  "    "
                  mode-line-misc-info
                  ;;mode-line-end-spaces
                  )))

;;; appearance.el ends here
