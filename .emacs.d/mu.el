;;; mu.el --- mu email config

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(use-package mu4e
  :config
  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; if rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.
  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "nan@notanumber.io" from) "NaN")
                 ((string-match "nzhong@digitalocean.com" from) "do"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-get-mail-command "/usr/local/bin/gtimeout --kill-after=5m 3m /usr/local/bin/mbsync -a")
  (setq mu4e-update-interval 300)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-html2text-command "w3m -dump -T text/html")
  ;; (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-skip-duplicates t)
  ;; (setq mu4e-headers-include-related t)
  (setq mu4e-headers-sort-direction 'ascending)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-attachment-dir  "~/Downloads")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-hide-index-messages t)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-user-mail-address-list
        (list "nanzhong@me.com" "nan@notanumber.io" "nzhong@digitalocean.com" "nan@digitalocean.com"))

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  ;; (add-hook 'mu4e-mark-execute-pre-hook
  ;;     (lambda (mark msg)
  ;;       (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
  ;;     ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
  ;;     ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "NaN"
             :enter-func (lambda () (mu4e-message "Switch to the NaN context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-maildir-matches msg "^/NaN")))
             :leave-func (lambda () (mu4e-clear-caches))
             :vars '((user-mail-address     . "nan@notanumber.io")
                     (user-full-name        . "Nan Zhong")
                     (mu4e-sent-folder      . "/NaN/[Gmail].Sent Mail")
                     (mu4e-drafts-folder    . "/NaN/[Gmail].Drafts")
                     (mu4e-trash-folder     . "/NaN/[Gmail].Trash")
                     (mu4e-refile-folder    . "/NaN/[Gmail].All Mail")))
           ,(make-mu4e-context
             :name "do"
             :enter-func (lambda () (mu4e-message "Switch to the do context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-maildir-matches msg "^/do")))
             :leave-func (lambda () (mu4e-clear-caches))
             :vars '((user-mail-address     . "nzhong@digitalocean.com")
                     (user-full-name        . "Nan Zhong")
                     (mu4e-sent-folder      . "/do/[Gmail].Sent Mail")
                     (mu4e-drafts-folder    . "/do/[Gmail].Drafts")
                     (mu4e-trash-folder     . "/do/[Gmail].Trash")
                     (mu4e-refile-folder    . "/do/[Gmail].All Mail")))))

  ;; sending mail
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/local/bin/msmtp"
        user-full-name "Nan Zhong")

  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

  (setq mu4e-bookmarks `(,(make-mu4e-bookmark
                           :name "inboxes"
                           :query "maildir:/NaN/INBOX OR maildir:/do/INBOX"
                           :key ?i)
                         ,(make-mu4e-bookmark
                           :name "do-github"
                           :query "maildir:/do/github"
                           :key ?g)
                         ,(make-mu4e-bookmark
                           :name "do-sentry"
                           :query "maildir:/do/sentry"
                           :key ?s)
                         ,(make-mu4e-bookmark
                           :name "do-jira"
                           :query "maildir:/do/jira"
                           :key ?j)
                         ,(make-mu4e-bookmark
                           :name "unread"
                           :query (concat "flag:unread "
                                          "AND NOT maildir:\"/NaN/[Gmail].All Mail\" "
                                          "AND NOT maildir:\"/NaN/[Gmail].Trash\" "
                                          "AND NOT maildir:\"/NaN/[Gmail].Spam\" "
                                          "AND NOT maildir:\"/do/[Gmail].Trash\" "
                                          "AND NOT maildir:\"/do/[Gmail].Spam\" "
                                          "AND NOT maildir:\"/do/[Gmail].All Mail\"")
                           :key ?u)
                         ,(make-mu4e-bookmark
                           :name "today"
                           :query "date:today..now"
                           :key ?t)
                         ,(make-mu4e-bookmark
                           :name "week"
                           :query "date:7d..now"
                           :key ?w))))

(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-interesting-mail-query (concat "flag:unread "
                                                  "AND NOT maildir:\"/NaN/[Gmail].All Mail\" "
                                                  "AND NOT maildir:\"/NaN/[Gmail].Trash\" "
                                                  "AND NOT maildir:\"/NaN/[Gmail].Spam\" "
                                                  "AND NOT maildir:\"/do/[Gmail].All Mail\" "
                                                  "AND NOT maildir:\"/do/[Gmail].Trash\" "
                                                  "AND NOT maildir:\"/do/[Gmail].Spam\""))
  (setq mu4e-alert-notify-repeated-mails t)
  (setq mu4e-alert-email-notification-types '(count))
  (setq mu4e-alert-modeline-formatter (lambda (count) (format " [Mail %d] " count))))

;;; mu.el ends here
