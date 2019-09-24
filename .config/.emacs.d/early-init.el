;;; early-init.el --- emacs config

;;; Commentary:

;;; Code:

;; Setup packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;;; early-init.el ends here
