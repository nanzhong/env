;;; nan-theme.el --- NaN Theme

;; Copyright 2018-present, all rights reserved.
;;
;; Code licensed under MIT licence.

;; Author: Nan Zhong <me@nanzho.ng>
;; Version: 0.2
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/nanzhong/workstation

;;; Commentary:

;; A simple theme

;;; Code:

(deftheme nan
  "A simple theme")

(let* ((black "#000000")
       (white "#ffffff")

       (bg    "#161a1d")
       (bg+   "#1e2529")
       (bg++  "#272f35")
       (bg+++ "#303a41")

       (fg    "#e2e6e9")
       (fg-   "#a7b4be")
       (fg--  "#6c8393")
       (fg--- "#414e58")

       (base-0  "#ed6a5e")
       (base-0+ "#f66355")
       (base-1  "#ed8e5e")
       (base-1+ "#f68b55")
       (base-2  "#edd55e")
       (base-2+ "#f6db55")
       (base-3  "#b2ed5e")
       (base-3+ "#b3f655")
       (base-4  "#5ebeed")
       (base-4+ "#55c1f6")
       (base-5  "#5e76ed")
       (base-5+ "#5570f6")
       (base-6  "#8e5eed")
       (base-6+ "#8b55f6")
       (base-7  "#ed5ea6")
       (base-7+ "#f655a6")

       ;; ansi colours
       (bright-black   black)
       (black          black)
       (bright-white   white)
       (white          white)
       (bright-red     base-0+)
       (red            base-0)
       (bright-green   base-3+)
       (green          base-3)
       (bright-yellow  base-2+)
       (yellow         base-2)
       (bright-blue    base-5+)
       (blue           base-5)
       (bright-magenta base-7+)
       (magenta        base-7)
       (bright-cyan    base-4+)
       (cyan           base-4))

  (custom-theme-set-faces
   'nan

   ;; Default
   `(default               ((t (:foreground ,fg :background ,bg))))
   `(italic                ((t (:italic t))))
   `(cursor                ((t (:background ,fg---))))
   `(ffap                  ((t (:foreground ,base-7))))
   `(fringe                ((t (:background ,bg+))))
   `(highlight             ((t (:background ,bg+))))
   `(line-number           ((t (:foreground ,fg--- :background ,bg+))))
   `(lazy-highlight        ((t (:background ,base-4))))
   `(link                  ((t (:foreground ,base-4 :underline t))))
   `(minibuffer-prompt     ((t (:foreground ,base-1))))
   `(region                ((t (:background ,bg+++))))
   `(show-paren-match-face ((t (:background ,base-6))))
   `(vertical-border       ((t (:foreground ,bg++ :background ,bg++))))
   `(warning               ((t (:foreground ,base-0))))

   ;; tty-menu
   `(tty-menu-enabled-face ((t (:foreground ,fg :background ,bg+))))
   `(tty-menu-disabled-face ((t (:foreground ,fg- :background ,bg+))))
   `(tty-menu-selected-face ((t (:foreground ,white :background ,bg++ :weight bold))))

   ;; header-line
   `(header-line ((t (:foreground ,fg :background ,bg+ :underline nil))))

   ;; mode-line
   `(mode-line ((t (:background ,bg++ :foreground ,fg :box nil))))
   `(mode-line-inactive ((t (:background ,bg+ :foreground ,fg- :box nil))))

   ;; nan-mode-line
   `(nan-mode-line-buffer-name ((t (:foreground ,base-4 :weight bold))))
   `(nan-mode-line-buffer-modified ((t (:foreground ,base-3))))
   `(nan-mode-line-buffer-read-only ((t (:foreground ,base-2))))
   `(nan-mode-line-vc ((t (:foreground ,base-6 :weight bold))))

   ;; flymake
   `(flymake-note ((t :background ,base-4 :foreground ,bg)))
   `(flymake-warning ((t :background ,base-1 :foreground ,bg)))
   `(flymake-error ((t (:background ,base-0 :foreground ,fg))))

   ;; Syntax highlighting
   `(font-lock-builtin-face ((t (:foreground ,base-7))))
   `(font-lock-comment-face ((t (:foreground ,fg-- :inherit italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-- :inherit italic))))
   `(font-lock-constant-face ((t (:foreground ,base-1))))
   `(font-lock-doc-face ((t (:foreground ,fg-- :inherit italic))))
   `(font-lock-function-name-face ((t (:foreground ,base-4))))
   `(font-lock-keyword-face ((t (:foreground ,base-7))))
   `(font-lock-negation-face ((t (:foreground ,base-0))))
   `(font-lock-preprocessor-face ((t (:foreground ,base-6))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,base-6))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,base-6))))
   `(font-lock-string-face ((t (:foreground ,base-1))))
   `(font-lock-type-face ((t (:foreground ,base-4))))
   `(font-lock-variable-name-face ((t (:foreground ,base-4))))
   `(font-lock-warning-face ((t (:foreground ,base-0))))

   ;; lsp
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,fg :background ,bg+ :underline nil))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:foreground ,fg :background ,bg+ :underline nil))))
   `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :foreground ,base-0))))
   `(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit lsp-headerline-breadcrumb-path-face :foreground ,base-1))))
   `(lsp-headerline-breadcrumb-path-info-face ((t (:inherit lsp-headerline-breadcrumb-path-face :foreground ,base-4))))
   `(lsp-headerline-breadcrumb-path-hint-face ((t (:inherit lsp-headerline-breadcrumb-path-face :foreground ,base-5))))
   `(lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit lsp-headerline-breadcrumb-path-face :foreground ,base-7 :weight bold))))
   `(lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit lsp-headerline-breadcrumb-project-prefix-face))))
   `(lsp-headerline-breadcrumb-symbols-face ((t (:inherit lsp-headerline-breadcrumb-path-face :weight bold))))
   `(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :foreground ,base-0))))
   `(lsp-headerline-breadcrumb-symbols-warning-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :foreground ,base-1))))
   `(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :foreground ,base-4))))
   `(lsp-headerline-breadcrumb-symbols-hint-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :foreground ,base-5))))

   ;; lsp-ui
   `(lsp-ui-doc-background ((t (:background ,bg+))))
   `(lsp-ui-sideline-code-action ((t (:foreground ,base-4 :background ,bg+ :weight bold))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t :foreground ,base-0)))
   `(rainbow-delimiters-depth-2-face ((t :foreground ,base-1)))
   `(rainbow-delimiters-depth-3-face ((t :foreground ,base-2)))
   `(rainbow-delimiters-depth-4-face ((t :foreground ,base-3)))
   `(rainbow-delimiters-depth-5-face ((t :foreground ,base-4)))
   `(rainbow-delimiters-depth-6-face ((t :foreground ,base-5)))
   `(rainbow-delimiters-depth-7-face ((t :foreground ,base-6)))
   `(rainbow-delimiters-depth-8-face ((t :foreground ,base-7)))
   `(rainbow-delimiters-unmatched-face ((t :foreground ,base-0 :underline ,base-0 :weight bold)))

   ;; corfu
   `(corfu-default ((t (:foreground ,fg :background ,bg+))))
   `(corfu-current ((t (:foreground ,white :background ,bg++ :bold t))))
   `(corfu-bar ((t (:background ,bg++))))
   `(corfu-border ((t (:background ,bg++))))

   ;; org-mode
   `(org-level-1 ((t (:foreground ,base-0 :weight bold))))
   `(org-level-2 ((t (:foreground ,base-1 :weight bold))))
   `(org-level-3 ((t (:foreground ,base-2 :weight bold))))
   `(org-level-4 ((t (:foreground ,base-3 :weight bold))))
   `(org-level-5 ((t (:foreground ,base-4 :weight bold))))
   `(org-level-6 ((t (:foreground ,base-5 :weight bold))))
   `(org-level-7 ((t (:foreground ,base-6 :weight bold))))
   `(org-level-8 ((t (:foreground ,base-7 :weight bold))))

   `(org-todo ((t (:foreground ,base-2 :weight bold))))
   `(org-done ((t (:foreground ,base-4 :weight bold))))

   `(org-imminent-deadline ((t (:foreground ,base-0))))
   `(org-upcoming-deadline ((t (:foreground ,base-1))))
   `(org-upcoming-distant-deadline ((t (:foreground ,base-2))))

   `(org-agenda-structure ((t (:foreground ,fg-- :weight bold))))
   `(org-agenda-date ((t (:foreground ,fg-))))
   `(org-agenda-date-today ((t (:foreground ,fg- :weight bold))))
   `(org-agenda-date-weekend ((t (:foreground ,fg-))))
   `(org-agenda-current-time ((t (:foreground ,base-4))))
   `(org-agenda-diary ((t (:foreground ,base-5))))
   `(org-time-grid ((t (:foreground ,fg--))))

   `(org-super-agenda-header ((t (:foreground ,base-7 :weight bold))))

   `(org-scheduled ((t (:foreground ,fg))))
   `(org-scheduled-previously ((t (:foreground ,base-0))))
   `(org-scheduled-today ((t (:foreground ,fg))))

   `(org-drawer ((t (:foreground ,fg---))))
   `(org-special-keyword ((t (:foreground ,base-0 :weight bold))))
   `(org-tag ((t (:foreground ,base-4 :weight bold))))
   `(org-ellipsis ((t (:foreground ,fg---))))
   `(org-warning ((t (:foreground ,base-0 :weight bold))))

   ;; diff-hl
   `(diff-hl-insert ((t :foreground ,base-3 :background ,base-3)))
   `(diff-hl-change ((t :foreground ,base-2 :background ,base-2)))
   `(diff-hl-delete ((t :foreground ,base-0, :background ,base-0)))

   ;; git
   `(git-commit-summary ((t (:foreground ,base-4 :weight bold))))

   ;; magit
   `(magit-branch ((t (:foreground ,base-6 :weight bold))))
   `(magit-branch-local ((t (:foreground ,base-6 :weight bold))))
   `(magit-branch-remote ((t (:foreground ,base-6 :weight bold))))
   `(magit-cherry-equivalent ((t (:foreground ,base-3 :weight bold))))
   `(magit-cherry-unmatched ((t (:foreground ,base-1 :weight bold))))
   `(magit-diff-added ((t (:foreground ,base-3))))
   `(magit-diff-added-highlight ((t (:foreground ,base-3 :background ,bg+))))
   `(magit-diff-context-highlight ((t (:background ,bg+))))
   `(magit-diff-file-header ((t (:foreground ,base-5))))
   `(magit-diff-removed ((t (:foreground ,base-0))))
   `(magit-diff-removed-highlight ((t (:foreground ,base-0 :background ,bg+))))
   `(magit-diffstat-added ((t (:foreground ,base-3))))
   `(magit-diffstat-removed ((t (:foreground ,base-0))))
   `(magit-hash ((t (:foreground ,base-6))))
   `(magit-head ((t (:foreground ,base-6)))
   `(magit-hunk-heading ((t (:foreground ,base-1))))
   `(magit-hunk-heading-highlight ((t (:foreground ,base-1 :background ,bg+))))
   `(magit-item-highlight ((t (:foreground ,base-1 :background ,bg+))))
   `(magit-log-author ((t (:foreground ,base-2))))
   `(magit-log-graph ((t (:foreground ,fg-))))
   `(magit-log-head-label-bisect-bad ((t (:foreground ,base-0 :box 1))))
   `(magit-log-head-label-bisect-good ((t (:foreground ,base-3 :box 1))))
   `(magit-log-head-label-default ((t (:foreground ,fg :box 1))))
   `(magit-log-head-label-local ((t (:foreground ,base-6 :box 1))))
   `(magit-log-head-label-remote ((t (:foreground ,base-6 :box 1))))
   `(magit-log-head-label-patches ((t (:foreground ,base-2 :box 1))))
   `(magit-log-head-label-tags ((t (:foreground ,base-7 :box 1))))
   `(magit-log-sha1 ((t (:foreground ,base-6 :box 1))))
   `(magit-process-ng ((t (:foreground ,base-0 :weight bold))))
   `(magit-process-ok ((t (:foreground ,base-3 :weight bold))))
   `(magit-reflog-amend ((t (:foreground ,base-1))))
   `(magit-reflog-checkout ((t (:foreground ,base-1))))
   `(magit-reflog-cherry-pick ((t (:foreground ,base-1))))
   `(magit-reflog-commit ((t (:foreground ,base-1))))
   `(magit-reflog-merge ((t (:foreground ,base-1))))
   `(magit-reflog-other ((t (:foreground ,base-1))))
   `(magit-reflog-rebase ((t (:foreground ,base-1))))
   `(magit-reflog-remote ((t (:foreground ,base-1))))
   `(magit-reflog-reset ((t (:foreground ,base-1))))
   `(magit-section-heading ((t (:foreground ,base-4 :weight bold))))
   `(magit-section-highlight ((t (:background ,bg+))))
   `(magit-section-title ((t (:foreground ,base-5 :weight bold))))

   ;; whitespace
   `(whitespace-tab ((t (:foreground ,fg--))))
   `(whitespace-line ((t (:background ,bg++))))

   ;; widgets
   `(widget-field ((t (:background ,bg++ :foreground ,fg))))
   `(widget-single-line-field ((t (:inherit widget-line))))
   `(widget-button ((t (:background ,bg+++ :foreground ,fg :weight bold))))
   `(widget-button-pressed ((t (:background ,bg++ :foreground ,fg- :weight bold))))
   `(widget-documentation ((t (:foreground ,base-3))))
   `(widget-inactive ((t (:foreground ,fg--))))

   ;; ansi colours
   `(ansi-color-black ((t (:foreground ,black))))
   `(ansi-color-white ((t (:foreground ,white))))
   `(ansi-color-red ((t (:foreground ,red))))
   `(ansi-color-green ((t (:foreground ,green))))
   `(ansi-color-yellow ((t (:foreground ,yellow))))
   `(ansi-color-blue ((t (:foreground ,blue))))
   `(ansi-color-magenta ((t (:foreground ,magenta))))
   `(ansi-color-cyan ((t (:foreground ,cyan))))
   `(ansi-color-bright-black ((t (:foreground ,bright-black))))
   `(ansi-color-bright-white ((t (:foreground ,bright-white))))
   `(ansi-color-bright-red ((t (:foreground ,bright-red))))
   `(ansi-color-bright-green ((t (:foreground ,bright-green))))
   `(ansi-color-bright-yellow ((t (:foreground ,bright-yellow))))
   `(ansi-color-bright-blue ((t (:foreground ,bright-blue))))
   `(ansi-color-bright-magenta ((t (:foreground ,bright-magenta))))
   `(ansi-color-bright-cyan ((t (:foreground ,bright-cyan)))))
   )

  (custom-theme-set-variables
   'nan
   `(ansi-color-names-vector
     [,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nan)
(provide 'nan-theme)

;;; nan-theme.el ends here
