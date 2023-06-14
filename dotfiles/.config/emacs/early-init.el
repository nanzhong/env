;;; early-init.el --- emacs config

;;; Commentary:

;;; Code:

;; Disable package.el since we are using straight.el
(setq package-enable-at-startup nil)

;; GUI tweaks for more minimal look
(setq default-frame-alist '((font . "Iosevka Custom-11")
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars)))

;;; early-init.el ends here
