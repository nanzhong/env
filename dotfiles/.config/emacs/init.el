;;; init.el --- emacs config

;;; Commentary:

;;; Code:

;; Increase gc threshold
;; (this value is a balancing act between overall performance and responsiveness)
(setq gc-cons-threshold (* 10 1024 1024))
;; language server responses are often much larger than the 4KB default
(setq read-process-output-max (* 10 1024 1024))

;; Boostrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap use-package
(straight-use-package 'use-package)

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

(setq custom-file "~/.config/emacs/custom.el")
(if (file-exists-p custom-file) (load custom-file))

;;; init.el ends here
