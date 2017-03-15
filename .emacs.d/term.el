;;; term.el --- terminal and eshell specific

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
;;(setenv "TERM" "xterm-256color")

;; use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

(use-package xterm-color
  :ensure t)

;; term related
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun term-fish ()
  "Opens a fish ansi-term."
  (interactive)
  (setq-local close-window-on-exit t)
  (ansi-term "/usr/local/bin/fish"))

(global-set-key (kbd "C-M-<return>") 'term-fish)

;; comint install
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

;; eshell related

(require 'eshell)

(defun eshell-mode-hook-fix-env ()
  (setq eshell-path-env (getenv "PATH")))

;;(add-hook 'eshell-mode-hook 'eshell-mode-hook-fix-env)

(defun my/truncate-eshell-buffers ()
  "Truncates all eshell buffers"
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))

;; After being idle for 5 seconds, truncate all the eshell-buffers if
;; needed. If this needs to be canceled, you can run `(cancel-timer
;; my/eshell-truncate-timer)'
(setq my/eshell-truncate-timer
      (run-with-idle-timer 10 t #'my/truncate-eshell-buffers))

(defun eshell-new ()
  "Open a new eshell."
  (interactive)
  (eshell 'Z))

(global-set-key (kbd "M-S-<return>") 'eshell-new)

(defun eshell/d (&rest args)
  (dired (pop args) "."))

(defun eshell/f (filename &optional dir)
  "Searches in the current directory for files that match the
given pattern. A simple wrapper around the standard 'find'
function."
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
(add-to-list 'eshell-visual-commands "npm")
(add-to-list 'eshell-visual-commands "gpg")
(add-to-list 'eshell-visual-commands "git-crypt")
(add-to-list 'eshell-visual-commands "fly")

(setq eshell-scroll-to-bottom-on-input t)

(add-hook 'eshell-mode-hook
          (lambda ()
	    (setenv "TERM" "xterm-256color")
            (setq-local xterm-color-preserve-properties t)
	    (make-local-variable 'eshell-preoutput-filter-functions)
	    (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
	    (setq-local eshell-output-filter-functions
			(remove 'eshell-handle-ansi-color
				eshell-output-filter-functions))))



(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#24282f")
	(blue "#61afef")
	(green "#98be65")
	(red "#ff6c6b")
	(comment "#687080"))
    (concat
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
(setq eshell-cmpl-cycle-completions nil)
