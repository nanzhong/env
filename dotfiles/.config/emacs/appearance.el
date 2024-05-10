;;; appearance.el --- theme and ui related config

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :after (minions highlight-indent-guides)
  :config
  (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
  (load-theme 'nan t)

  ;; GUI
  (fringe-mode 10)
  (setq frame-title-format nil
        frame-resize-pixelwise t)
  (setq-default cursor-type 'bar)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

  ;; TTY
  (xterm-mouse-mode 1)

  ;; No bell
  (setq visible-bell t
        ring-bell-function 'ignore)

  ;; Maximum styles
  (setq font-lock-maximum-decoration t)

  ;; Allow fitting window width
  (setq fit-window-to-buffer-horizontally t)

  ;; Set a different character for the vertical border and wrap
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (let ((display-table (or buffer-display-table standard-display-table)))
                (set-display-table-slot display-table 'vertical-border ?│)
                (set-display-table-slot display-table 'wrap ?↩))))

  ;; Highlight current line
  (global-hl-line-mode 1)

  ;; Highlight matching parentheses when the point is on them.
  (show-paren-mode 1)

  ;; Enable column mode
  (column-number-mode 1)

  ;; Toggle truncate-lines
  (global-set-key (kbd "C-c $") 'toggle-truncate-lines))

(use-package nan-prog-env
  :ensure nil
  :demand t
  :after (whitespace highlight-indent-guides))

(use-package nan-mode-line
  :ensure nil
  :after minions
  :demand t)

(use-package minions
  :demand t
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "⍠"
        minions-mode-line-delimiters '("" . "")
        minions-prominent-modes '(flymake-mode)))

(use-package whitespace
  :ensure nil
  :demand t
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs tab-mark indentation lines-tail)))

;; Indent guides
(use-package highlight-indent-guides
  :demand t
  :config (setq highlight-indent-guides-method 'column
                highlight-indent-guides-auto-odd-face-perc 30
                highlight-indent-guides-auto-even-face-perc 25))

(use-package rainbow-delimiters
  :demand t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; appearance.el ends here
