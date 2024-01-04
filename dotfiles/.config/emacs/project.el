;;; project.el --- project.el related

;;; Commentary:

;;; Code:

(use-package project
  :elpaca nil
  :config
  (setq project-root-markers
        '("package.json"))

  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  (add-to-list 'project-find-functions #'project-find-root))

;;; project.el ends here
