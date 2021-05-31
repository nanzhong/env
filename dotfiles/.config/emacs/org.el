;;; -*- lexical-binding: t; -*-
;;; org.el --- org specific

;;; Commentary:

;;; Code:

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-store-link)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out))
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  :config
  (defun org-find-heading-in-datetree (heading)
    (lambda ()
      (org-datetree-find-date-create (calendar-current-date))
      (goto-char (point-at-eol))
      (when (not (re-search-forward
                  (format org-complex-heading-regexp-format
                          (regexp-quote heading)) nil t))
        (insert (concat "\n**** " heading)))))
  (setq org-directory "~/org"
        org-default-notes-file "~/org/nan.org"
        org-agenda-files (list (concat org-directory "/nan.org")
                               (concat org-directory "/do.org"))
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r!)" "BLOCKED(b@/!)" "|" "DONE(d/!)" "REJECTED(k@)"))
        org-log-into-drawer "LOGBOOK"
        org-startup-indented t
        org-id-link-to-org-use-id t
        org-catch-invisible-edits 'smart
        org-cycle-separator-lines 1
        org-capture-templates `(("h" "Health")
                                ("hw" "Weight"
                                 plain (file+function ,(concat org-directory "/health.org") ,(org-find-heading-in-datetree "Weight"))
                                 ":PROPERTIES:\n:VALUE: %^{Weight}\n:END:\n%?"
                                 :jump-to-captured t)
                                ("hf" "Food Log"
                                 entry (file+function ,(concat org-directory "/health.org") ,(org-find-heading-in-datetree "Food"))
                                 "* %?"
                                 :jump-to-captured t)
                                ("t" "Tasks")
                                ("tn" "Personal Task"
                                 entry (file+headline ,(concat org-directory "/nan.org") "Tasks")
                                 "* TODO %?")
                                ("td" "DigitalOcean Task"
                                 entry (file+headline ,(concat org-directory "/do.org") "Tasks")
                                 "* TODO %?\n:PROPERTIES:\n:JIRA: %^{Jira}\n:END:\n")

                                ("r" "Review")
                                ("rn" "Weekly Review - Personal"
                                 entry (file+olp+datetree ,(concat org-directory "/nan-review.org"))
                                 "* %?"
                                 :treetype week
                                 :jump-to-captured t)
                                ("rd" "Weekly Review - DigitalOcean"
                                 entry (file+olp+datetree ,(concat org-directory "/do-review.org"))
                                 "* Good\n%?\n* Bad\n* Change\n* Tasks\n** Completed\n** Next\n"
                                 :treetype week
                                 :jump-to-captured t))
        org-src-preserve-indentation t))

(use-package org-agenda-property
  :straight t
  :commands org-agenda
  :config
  (setq org-agenda-property-list '("LOCATION")))

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam" :branch "v2")
  :after org
  :commands (org-roam-setup
             org-roam-buffer
             org-roam-capture
             org-roam-node-find
             org-roam-node-insert)
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c r s" . org-roam-db-sync))
  :config
  (setq org-roam-directory "~/org"
        org-roam-file-extensions '("org"))
  (org-roam-setup))

;; (use-package org-roam-server
;;   :straight t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 9999
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

(use-package htmlize
  :straight t
  :config
  (setq org-html-htmlize-output-type 'css))

;;; org.el ends here
