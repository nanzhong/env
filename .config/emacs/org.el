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
                             (concat org-directory "/eva.org")
                             (concat org-directory "/todo.org")
                             (concat org-directory "/cal/do.org")
                             (concat org-directory "/cal/nan.org")
                             (concat org-directory "/cal/icloud.org")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-capture-templates
      `(("t" "Task templates")
        ("tp" "Personal task"
         entry (file+headline ,(concat org-directory "/todo.org") "Personal")
         "* %?")
        ("td" "DigitalOcean task"
         entry (file+headline ,(concat org-directory "/todo.org") "DigitalOcean")
         "* %?")

        ("n" "Note"
         entry (file ,(concat org-directory "/notes.org"))
         "* %?\n%U")

        ("e" "Eva related templates")
        ("et" "Eva tracking"
         entry (file+headline ,(concat org-directory "/eva.org") "Tracking")
         "* %?\n:PROPERTIES:\n:Application: %^{Application|Google Doc|OneNote|n/a}\n:END:\n%T"
         :empty-lines 1)))

;; Re-define org-switch-to-buffer-other-window to NOT use org-no-popups.
;; Primarily for compatibility with shackle.
(defun org-switch-to-buffer-other-window (args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer.
Redefined to allow pop-up windows."
  ;;  (org-no-popups
  ;;     (apply 'switch-to-buffer-other-window args)))
  (switch-to-buffer-other-window args))

(use-package org-agenda-property
  :ensure t
  :commands org-agenda
  :config
  (setq org-agenda-property-list '("LOCATION" "Application")))

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
