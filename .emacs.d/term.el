;;; term.el --- term specific

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun term-fish ()
  "Opens a fish ansi-term."
  (interactive)
  (setq-local close-window-on-exit t)
  (ansi-term "/usr/local/bin/fish"))

(global-set-key (kbd "C-M-<return>") 'term-fish)

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

;; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)
