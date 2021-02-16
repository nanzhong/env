;;; init.el --- emacs config

;;; Commentary:

;;; Code:

;; Increase gc threshold
;; (this value is a balancing act between overall performance and responsiveness)
(setq gc-cons-threshold (* 10 1024 1024))
;; language server responses are often much larger than the 4KB default
(setq read-process-output-max (* 10 1024 1024))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load configs
(load (expand-file-name "appearance.el" user-emacs-directory))
(load (expand-file-name "core.el" user-emacs-directory))
(load (expand-file-name "lsp.el" user-emacs-directory))
(load (expand-file-name "treemacs.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))
(load (expand-file-name "elisp.el" user-emacs-directory))
(load (expand-file-name "ruby.el" user-emacs-directory))
(load (expand-file-name "go.el" user-emacs-directory))
(load (expand-file-name "swift.el" user-emacs-directory))
(load (expand-file-name "js.el" user-emacs-directory))
(load (expand-file-name "web.el" user-emacs-directory))
(load (expand-file-name "git.el" user-emacs-directory))
(load (expand-file-name "term.el" user-emacs-directory))
(load (expand-file-name "spelling.el" user-emacs-directory))
(load (expand-file-name "misc-modes.el" user-emacs-directory))

;(load (expand-file-name "do.el" user-emacs-directory))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; disabled commands
(put 'downcase-region 'disabled nil)

;;; init.el ends here
