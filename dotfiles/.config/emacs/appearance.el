;;; appearance.el --- theme and ui related config

;;; Commentary:

;;; Code:

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'nan t)

(if window-system
    (progn (fringe-mode 5)
           (setq frame-title-format nil)
           (add-to-list 'default-frame-alist '(font . "Iosevka 12"))
           (setq-default cursor-type 'bar))
  (xterm-mouse-mode 1))

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
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;; Indent guides
(use-package highlight-indent-guides
  :straight t
  :config (setq highlight-indent-guides-method 'column
                highlight-indent-guides-auto-odd-face-perc 30
                highlight-indent-guides-auto-even-face-perc 25))

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs tab-mark indentation lines-tail)))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun setup-prog-env ()
  "Setup prog like environment."
  (display-line-numbers-mode)
  ;; From highlight-indent-guides
  (highlight-indent-guides-mode)
  ;; From whitespace
  (whitespace-mode)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace 1))

(require 'derived)
(dolist (hook (mapcar #'derived-mode-hook-name prog-like-modes))
  (add-hook hook 'setup-prog-env))

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
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,(length formatted-right)))))
            formatted-right)))

(defun nan-mode-line-buffer-name ()
  "mode-line segment for displaying buffer name."
  (propertize " %b " 'face 'nan-mode-line-buffer-name))

(defun nan-mode-line-buffer-properties ()
  "mode-line segment for displaying buffer properties."
  (concat
   (if (buffer-modified-p) (propertize "█" 'face 'nan-mode-line-buffer-modified) "░")
   (if (and (buffer-file-name) buffer-read-only) (propertize " " 'face 'nan-mode-line-buffer-read-only) "")))

(defun nan-mode-line-buffer-eol ()
  "mode-line segment for displaying the buffer's eol style."
  (pcase (coding-system-eol-type buffer-file-coding-system)
    (0 "␊")
    (1 "␍␊")
    (2 "␍")))

(defun nan-mode-line-buffer-position ()
  "mode-line segment for displaying the position in the buffer."
  (concat (propertize "@" 'face 'nan-mode-line-unimportant)
          " %l:%c"
          ;; (propertize " %p%%" 'face 'nan-mode-line-unimportant)
          (propertize (format ":%d" (point)) 'face 'nan-mode-line-unimportant)))

(defun nan-mode-line-vc ()
  "mode-line segment for displaying the vc info."
  (when vc-mode (concat (propertize " " 'face 'nan-mode-line-vc)
                        (propertize (string-trim vc-mode) 'face 'nan-mode-line-vc))))

(defun nan-mode-line-misc-info ()
  "mode-line segment for displaying misc info."
  (string-trim (format-mode-line mode-line-misc-info)))

(defun nan-mode-line-modes ()
  "mode-line segment for displaying modes."
  (string-trim (format-mode-line minions-mode-line-modes)))

(setq-default mode-line-format
              '((:eval (nan-mode-line-format
                        ;; Left
                        `((:eval (nan-mode-line-buffer-properties))
                          " "
                          (:eval (nan-mode-line-buffer-name))
                          " "
                          (:eval (nan-mode-line-buffer-eol))
                          " "
                          (:eval (nan-mode-line-buffer-position))
                          " "
                          (:eval (nan-mode-line-vc)))
                        ;; Right
                        `((:eval (nan-mode-line-misc-info))
                          " "
                          (:eval (nan-mode-line-modes)))))))

;;; appearance.el ends here
