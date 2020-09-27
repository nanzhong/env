;;; -*- lexical-binding: t; -*-
;;; org.el --- org specific

;;; Commentary:

;;; Code:

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-link))
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
        (insert (concat "\n**** " heading "\n")))
      (goto-char (point-at-eol))))
  (setq org-directory "~/org"
        org-default-notes-file "~/org/nan.org"
        org-agenda-files (list (concat org-directory "/nan.org")
                               (concat org-directory "/do.org"))
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b@/!)" "|" "DONE(d!)" "REJECTED(r@)"))
        org-log-into-drawer "LOGBOOK"
        org-startup-indented t
        org-capture-templates
        `(("h" "Health")
          ("he" "Weight"
           plain (file+function ,(concat org-directory "/health.org") ,(org-find-heading-in-datetree "Weight"))
           ":PROPERTIES:\n:VALUE: %^{Weight (kg)}\n:END:\n%?"
           :jump-to-captured t)
          ("hf" "Food Log"
           entry (file+function ,(concat org-directory "/health.org") ,(org-find-heading-in-datetree "Food"))
           "* %?"
           :jump-to-captured t)
          ("t" "Tasks")
          ("tp" "Personal Task"
           entry (file+headline ,(concat org-directory "/nan.org") "Personal")
           "* TODO %?")
          ("td" "DigitalOcean Task"
           entry (file+headline ,(concat org-directory "/do.org") "DigitalOcean")
           "* TODO %?")

          ("r" "Review")
          ("rn" "Weekly review - Personal"
           entry (file+olp+datetree ,(concat org-directory "/nan-review.org"))
           "* %?"
           :treetype week
           :jump-to-captured t)
          ("rd" "Weekly Review - DigitalOcean"
           entry (file+olp+datetree ,(concat org-directory "/do-review.org"))
           "* %?"
           :treetype week
           :jump-to-captured t))))

(use-package org-agenda-property
  :ensure t
  :commands org-agenda
  :config
  (setq org-agenda-property-list '("LOCATION")))

;; (use-package org-jira
;;   :ensure t
;;   :config
;;   (setq jiralib-url "https://jira.internal.digitalocean.com"
;;         org-jira-use-status-as-todo t
;;         org-jira-working-dir "~/org/jira"
;;         org-jira-done-states '("Closed" "Resolved" "Done"
;;                                "Duplicate" "Rejected"
;;                                "Working as Intended" "Cannot Reproduce" "Fix Verified")))

;;; org.el ends here
