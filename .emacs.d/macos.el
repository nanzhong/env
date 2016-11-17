;;; macos.el --- mac specific

(use-package exec-path-from-shell
  :ensure t
  :if (equal system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))
