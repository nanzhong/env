;;; org.el --- org specific

;;; Commentary:

;;; Code:

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/notes.org")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-agenda-files (list (concat org-directory "/notes.org")
                             (concat org-directory "/todo.org")
                             (concat org-directory "/cal/do.org")
                             (concat org-directory "/cal/nan.org")
                             (concat org-directory "/cal/icloud.org")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(add-hook 'org-mode-hook 'org-indent-mode)

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
