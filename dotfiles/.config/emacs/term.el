;;; term.el --- terminal and eshell specific

;;; Commentary:

;;; Code:

(use-package eshell
  :config
  (defun eshell-new ()
    "Open a new eshell."
    (interactive)
    (eshell 'Z))

  (global-set-key (kbd "M-S-<return>") 'eshell-new)

  (defun eshell/d (&rest args)
    (dired (pop args) "."))

  (defun eshell/f (filename &optional dir)
    "Searches in the current directory for files that match the given pattern.
A simple wrapper around the standard 'find' function."
    (let ((cmd (concat
                "find " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path '*node_modules*'"
                " -and -not -path '*classes*'"
                " -and "
                " -type f -and "
                "-iname '" filename "'")))
      (message cmd)
      (shell-command-to-string cmd)))

  (defun eshell/ef (filename &optional dir)
    "Searches for the first matching filename and loads it into a
file to edit."
    (let* ((files (eshell/f filename dir))
           (file (car (s-split "\n" files))))
      (find-file file)))

  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  (defalias 'ff 'find-file)
  (defalias 'gd 'magit-diff-unstaged)
  (defalias 'gds 'magit-diff-staged)

  (require 'em-term)
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "gpg")
  (add-to-list 'eshell-visual-commands "git-crypt")
  (add-to-list 'eshell-visual-commands "fly")

  (setq eshell-scroll-to-bottom-on-input t)

  (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

  (defun shk-eshell-prompt ()
    (let ((header-bg "#292a2b")
          (blue "#7dc1ff")
          (green "#63de5d")
          (red "#ec2864")
          (comment "#676b79"))
      (concat
       "\n"
       (with-face (concat (eshell/pwd) " ") :background header-bg)
       (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground comment)
       (with-face "\n" :background header-bg)
       (with-face user-login-name :foreground blue)
       "@"
       (with-face "lime" :foreground green)
       (if (= (user-uid) 0)
           (with-face " #" :foreground red)
         " $")
       " ")))
  (setq eshell-prompt-function 'shk-eshell-prompt)
  (setq eshell-highlight-prompt nil)
  (setq eshell-cmpl-cycle-completions nil))

(use-package esh-autosuggest
  :straight t
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package multi-term
  :straight t
  :config
  (setq multi-term-program "/usr/local/bin/fish")
  (setq multi-term-program-switches "--login")
  (setq multi-term-scroll-to-bottom-on-output nil))

;; disable hl-line and wrapping for term like modes
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)
                              (toggle-truncate-lines)))
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)
                            (toggle-truncate-lines)))

;;; term.el ends here
