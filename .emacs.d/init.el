;;; init.el --- emacs config

;; Setup packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load configs
(load (expand-file-name "appearance.el" user-emacs-directory))
(load (expand-file-name "core.el" user-emacs-directory))
(load (expand-file-name "ruby.el" user-emacs-directory))
(load (expand-file-name "go.el" user-emacs-directory))
(load (expand-file-name "swift.el" user-emacs-directory))
(load (expand-file-name "lua.el" user-emacs-directory))
(load (expand-file-name "web.el" user-emacs-directory))
(load (expand-file-name "mu.el" user-emacs-directory))
(load (expand-file-name "git.el" user-emacs-directory))
(load (expand-file-name "eshell.el" user-emacs-directory))
(load (expand-file-name "term.el" user-emacs-directory))
(load (expand-file-name "flycheck.el" user-emacs-directory))
(load (expand-file-name "spelling.el" user-emacs-directory))
(load (expand-file-name "misc-modes.el" user-emacs-directory))
