;;; early-init.el --- emacs config

;;; Commentary:

;;; Code:

;; Disable package.el since we are using straight.el
(setq package-enable-at-startup nil)

;; GUI tweaks for more minimal look
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;;; early-init.el ends here
