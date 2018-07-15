;;; org.el --- org specific

;;; Commentary:

;;; Code:

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/notes.org")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-refile-targets (quote (("nan.org" :maxlevel . 9)
                                 ("do.org" :maxlevel . 9))))

(setq org-agenda-files (list (concat org-directory "/nan.org")
                             (concat org-directory "/do.org")))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-jira
  :ensure t
  :config
  (setq jiralib-url "https://jira.internal.digitalocean.com"
        org-jira-use-status-as-todo t
        org-jira-working-dir "~/org/jira"
        org-jira-done-states '("Closed" "Resolved" "Done"
                               "Duplicate" "Rejected"
                               "Working as Intended" "Cannot Reproduce" "Fix Verified")))

;;; org.el ends here
