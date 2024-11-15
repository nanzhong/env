;;; nan-mode-line.el --- My personalized mode-line

;; Copyright (C) 2023 Nan Zhong

;; Author: Nan Zhong <me@nanzho.ng>
;; Version: 0.0.1
;; Package-Requires: (minions)
;; Keywords: mode-line

;;; Commentary:

;;; Code:

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

(require 'minions)
(defun nan-mode-line-modes ()
  "mode-line segment for displaying modes."
  (string-trim (format-mode-line minions-mode-line-modes)))

(setq-default mode-line-format
              '((:eval (nan-mode-line-format
                        ;; Left
                        `((:eval (nan-mode-line-buffer-properties))

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

(provide 'nan-mode-line)

;;; nan-mode-line.el ends here
