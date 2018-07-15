;;; init.el --- emacs config

;;; Commentary:

;;; Code:

;; Emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; Increase gc threshold
;; (this value is a balancing act between overall performance and responsiveness)
(setq gc-cons-threshold (* 1 1024 1024))

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

;; icons-in-terminal
(add-to-list 'load-path "~/src/icons-in-terminal/build")

;; Common lisp extensions
(use-package cl
  :ensure t)

;; Load configs
(load (expand-file-name "core.el" user-emacs-directory))
(load (expand-file-name "appearance.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))
(load (expand-file-name "elisp.el" user-emacs-directory))
;(load (expand-file-name "gnus.el" user-emacs-directory))
(load (expand-file-name "ruby.el" user-emacs-directory))
(load (expand-file-name "go.el" user-emacs-directory))
(load (expand-file-name "swift.el" user-emacs-directory))
(load (expand-file-name "js.el" user-emacs-directory))
(load (expand-file-name "web.el" user-emacs-directory))
;(load (expand-file-name "mu.el" user-emacs-directory))
(load (expand-file-name "git.el" user-emacs-directory))
(load (expand-file-name "term.el" user-emacs-directory))
(load (expand-file-name "spelling.el" user-emacs-directory))
(load (expand-file-name "misc-modes.el" user-emacs-directory))

;(load (expand-file-name "do.el" user-emacs-directory))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; disabled commands
(put 'downcase-region 'disabled nil)

;;; init.el ends here
