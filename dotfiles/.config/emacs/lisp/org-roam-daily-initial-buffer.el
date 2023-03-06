;;; org-roam-daily-initial-buffer.el --- Set the initial buffer to today's org roam daily

;; Copyright (C) 2023 Nan Zhong

;; Author: Nan Zhong <me@nanzho.ng>
;; Version: 0.0.1
;; Package-Requires: ((org-roam "2.2.2"))
;; Keywords: mode-line

;;; Commentary:

;;; Code:

(require 'org-roam)
(setq initial-buffer-choice (lambda ()
                              (org-roam-dailies-find-today)
                              (get-buffer (format-time-string "%Y-%m-%d.org"))))

(provide 'org-roam-daily-initial-buffer)

;;; org-roam-daily-initial-buffer.el ends here
