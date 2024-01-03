;;; init.el --- emacs config

;;; Commentary:

;;; Code:

;; Increase gc threshold
;; (this value is a balancing act between overall performance and responsiveness)
(setq gc-cons-threshold (* 10 1024 1024))
;; language server responses are often much larger than the 4KB default
(setq read-process-output-max (* 10 1024 1024))

(setq native-comp-deferred-compilation-deny-list nil)

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
;; Emacs built from emacs-overlay does not include build time info, we manually set it to the current date
(defvar elpaca--core-date (list (string-to-number (format-time-string "%Y%m%d"))))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;; Load configs
(load (expand-file-name "core.el" user-emacs-directory))
(load (expand-file-name "project.el" user-emacs-directory))
(load (expand-file-name "appearance.el" user-emacs-directory))
(load (expand-file-name "eglot.el" user-emacs-directory))
(load (expand-file-name "treemacs.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))
(load (expand-file-name "lisp.el" user-emacs-directory))
(load (expand-file-name "elisp.el" user-emacs-directory))
(load (expand-file-name "ruby.el" user-emacs-directory))
(load (expand-file-name "go.el" user-emacs-directory))
(load (expand-file-name "zig.el" user-emacs-directory))
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
