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

;; Toggle truncate-lines
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;; Indent guides
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'column))

;; Modeline

(defface nan-mode-line-buffer-name
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying the buffer name.")

(defface nan-mode-line-buffer-modified
  '((t (:inherit shadow)))
  "Face used for displaying the modified buffer property.")

(defface nan-mode-line-buffer-read-only
  '((t (:inherit shadow)))
  "Face used for displaying the read-only buffer property.")

(defface nan-mode-line-vc
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying vc info.")

(defface nan-mode-line-unimportant
  '((t (:inherit (shadow))))
  "Face used for displaying less important mode-line info.")

(defun nan-mode-line-format (left right)
  "Returns a mode-line-format with left and right aligned segments."
  (let ((formatted-left (format-mode-line left))
        (formatted-right (format-mode-line right)))
    (concat formatted-left
            (propertize " " 'display `((space :align-to (- right ,(length formatted-right)))))
            formatted-right)))

(defun nan-mode-line-buffer-name ()
  "mode-line segment for displaying buffer name."
  (propertize "⌈%b⌋" 'face 'nan-mode-line-buffer-name))

(defun nan-mode-line-buffer-properties ()
  "mode-line segment for displaying buffer properties."
  (concat
   (if (buffer-modified-p) (propertize "█" 'face 'nan-mode-line-buffer-modified) "░")
   (if (and (buffer-file-name) buffer-read-only) (propertize "" 'face 'nan-mode-line-buffer-read-only) " ")))

(defun nan-mode-line-buffer-eol ()
  "mode-line segment for displaying the buffer's eol style."
  (pcase (coding-system-eol-type buffer-file-coding-system)
    (0 "␊")
    (1 "␍␊")
    (2 "␍")))

(defun nan-mode-line-buffer-position ()
  "mode-line segment for displaying the position in the buffer."
  (concat (propertize "@" 'face 'nan-mode-line-unimportant)
          "%l:%c"
          ;; (propertize " %p%%" 'face 'nan-mode-line-unimportant)
          (propertize (format ":%d" (point)) 'face 'nan-mode-line-unimportant)))

(defun nan-mode-line-vc ()
  "mode-line segment for displaying the vc info."
  (when vc-mode (concat (propertize " " 'face 'nan-mode-line-vc)
                        (propertize (string-trim vc-mode) 'face 'nan-mode-line-vc))))

(defun nan-mode-line-misc-info ()
  "mode-line segment for displaying misc info."
  (string-trim (format-mode-line mode-line-misc-info)))

(defun nan-mode-line-process ()
  "mode-line segment for displaying process info."
  (string-trim (format-mode-line mode-line-process)))

(setq-default mode-line-format
              '((:eval (nan-mode-line-format
                        ;; Left
                        `((:eval (nan-mode-line-buffer-properties))
                          " "
                          (:eval (nan-mode-line-buffer-name))
                          " "
                          (:eval (nan-mode-line-buffer-eol))
                          " "
                          (:eval (nan-mode-line-buffer-position)))
                        ;; Right
                        `((:eval (nan-mode-line-misc-info))
                          " "
                          (:eval (nan-mode-line-process))
                          " "
                          (:eval (nan-mode-line-vc))
                          " "
                          minions-mode-line-modes)))))

;;; appearance.el ends here
