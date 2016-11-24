;;; appearance.el --- theme and ui related config
(setq default-frame-alist '((font . "Menlo 10")))
(set-face-bold-p 'bold nil)

;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun set-frame-menu-bar-lines (frame)
  (let ((want-menu (memq (framep frame) '(x w32 ns))))
    (set-frame-parameter frame 'menu-bar-lines (if want-menu 1 0))))
(add-hook 'after-make-frame-functions 'set-frame-menu-bar-lines)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/nine27-theme")

(load-theme 'nine27 t)

(setq visible-bell t
      ring-bell-function 'ignore
      font-lock-maximum-decoration t
      color-theme-is-global t)

;; Highlight current line
(global-hl-line-mode 1)

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

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))
