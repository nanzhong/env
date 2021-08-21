;;; -*- lexical-binding: t; -*-
;;; org.el --- org specific

;;; Commentary:

;;; Code:

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out))
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  :config
  (setq org-directory "~/org"
        org-default-notes-file "~/org/notes.org"
        org-agenda-files '("~/org")
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r!)" "BLOCKED(b@/!)" "|" "DONE(d/!)" "REJECTED(k@)"))
        org-log-into-drawer "LOGBOOK"
        org-id-link-to-org-use-id t
        org-catch-invisible-edits 'smart
        org-capture-templates `(("t" "Tasks")
                                ("tn" "Personal Task"
                                 entry (file+olp ,(concat org-directory "/main.org") "Me" "Tasks")
                                 "* TODO %?")
                                ("td" "DigitalOcean Task"
                                 entry (file+olp ,(concat org-directory "/main.org") "DigitalOcean" "Tasks")
                                 "* TODO %?\n:PROPERTIES:\n:JIRA: %^{Jira}\n:END:\n")

                                ("r" "Review")
                                ("rw" "Weekly Review"
                                 entry (file+olp+datetree ,(concat org-directory "/review.org"))
                                 "* Good\n%?\n* Bad\n* Change\n* Tasks\n** Completed\n** Next\n"
                                 :treetype week
                                 :jump-to-captured t))
        org-src-preserve-indentation t
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

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
  :straight t
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
         ("C-c r s" . org-roam-db-sync)
         ;; Dailies
         ("C-c r d c" . org-roam-dailies-capture-today)
         ("C-c r d t" . org-roam-dailies-find-today)
         :map org-mode-map
         ("M-n" . org-roam-dailies-find-next)
         ("M-p" . org-roam-dailies-find-previous))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/org"
        org-roam-dailies-directory "daily/"
        org-roam-file-extensions '("org")
        org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "${slug}.org"
                                                         "#+title: ${title}\n")
                                      :unnarrowed t))
        org-roam-dailies-capture-templates '(("d" "default" entry
                                              "* %?"
                                              :if-new (file+head "%<%Y-%m-%d>.org"
                                                                 "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package htmlize
  :straight t
  :config
  (setq org-html-htmlize-output-type 'css))

;;; org.el ends here
