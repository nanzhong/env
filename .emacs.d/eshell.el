;;; eshell.el --- eshell specific

(defun eshell-mode-hook-fix-env ()
  (setq eshell-path-env (getenv "PATH")))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-fix-env)

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
