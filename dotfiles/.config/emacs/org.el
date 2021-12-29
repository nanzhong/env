;;; -*- lexical-binding: t; -*-
;;; org.el --- org specific

;;; Commentary:

;;; Code:

(use-package org
  :straight t
  :demand t
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
        org-agenda-files '("~/org" "~/org/dailies/" "~/org/1x1")
        org-agenda-include-diary t
        org-agenda-custom-commands '(("d" "Daily agenda"
                                      ((agenda "" ((org-agenda-overriding-header "")
                                                   (org-agenda-span 1)
                                                   (org-agenda-start-on-weekday nil)))
                                       (todo "BLOCKED" ((org-agenda-overriding-header "Blocked")))
                                       (todo "REVIEW" ((org-agenda-overriding-header "In Review"))))))
        org-startup-indented t
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r!)" "BLOCKED(b@/!)" "|" "DONE(d/!)" "REJECTED(k@)"))
        org-log-into-drawer "LOGBOOK"
        org-id-link-to-org-use-id t
        org-catch-invisible-edits 'smart
        org-src-preserve-indentation t
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package org-super-agenda
  :straight t
  :config
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Next"
                                         :priority<= "B")
                                  (:name "Blocked"
                                         :todo "BLOCKED")
                                  (:name "In Review"
                                         :todo "REVIEW")))
  (org-super-agenda-mode))

(use-package org-roam
  :straight t
  :requires org
  :demand t
  :commands (org-roam-setup
             org-roam-buffer
             org-roam-capture
             org-roam-node-find
             org-roam-node-insert
             org-roam-dailies-capture-today
             org-roam-dailies-find-today)
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c r s" . org-roam-db-sync)
         ;; Dailies
         ("C-c r d c" . org-roam-dailies-capture-today)
         ("C-c r d t" . org-roam-dailies-find-today)
         :map org-mode-map
         ("M-n" . org-roam-dailies-goto-next-note)
         ("M-p" . org-roam-dailies-goto-previous-note))
  :config
  (setq org-roam-directory "~/org"
        org-roam-dailies-directory "dailies/"
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
