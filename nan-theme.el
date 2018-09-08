;;; nan-theme.el --- NaN Theme

;; Copyright 2018-present, all rights reserved.
;;
;; Code licensed under MIT licence.

;; Author: Nan Zhong <nan@notanumber.io>
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/nanzhong/emacs-nan-theme

;;; Commentary:

;; A simple theme

;;; Code:

(deftheme nan
  "A simple theme")

(let ((bg--   "#000000")
      (bg-    "#121212")
      (bg     "#262626")
      (bg+    "#303030")
      (bg++   "#5f5f5f")

      (fg---  "#4e4e4e")
      (fg--   "#6c6c6c")
      (fg-    "#767676")
      (fg     "#eeeeee")
      (fg+    "#ffffff")

      (pink   "#ff87d7")
      (purple "#af87ff")
      (blue   "#87afff")
      (cyan   "#5fffd7")
      (green  "#5fd75f")
      (yellow "#ffd787")
      (orange "#ffaf5f")
      (red    "#ff005f"))

  (custom-theme-set-faces
   'nan

   ;; Default
   `(default ((t (:background ,bg :foreground ,fg))))
   `(italic ((t (:italic t))))
   `(cursor ((t (:background ,fg+))))
   `(ffap ((t :foreground ,fg+)))
   `(fringe ((t (:background ,bg))))
   `(highlight ((t (:background ,bg+))))
   `(line-number ((t :foreground ,fg---)))
   `(lazy-highlight ((t (:background ,orange))))
   `(link ((t (:foreground ,blue :underline t))))
   `(minibuffer-prompt ((t :foreground ,pink)))
   `(region ((t (:background ,bg++))))
   `(show-paren-match-face ((t (:background ,red))))
   `(trailing-whitespace ((t :foreground nil :background ,red)))
   `(vertical-border ((t (:foreground ,bg+ :background ,bg+))))
   `(warning ((t (:foreground ,orange))))

   ;; modeline
   `(mode-line ((t (:background ,bg++ :box nil))))
   `(mode-line-inactive ((t (:background ,bg+ :box nil))))

   ;; flycheck
   `(flycheck-info ((t :underline ,cyan)))
   `(flycheck-warning ((t :underline ,orange)))
   `(flycheck-error ((t :underline ,red)))
   `(flycheck-fringe-info ((t :foreground ,cyan)))
   `(flycheck-fringe-warning ((t :foreground ,orange)))
   `(flycheck-fringe-error ((t :foreground ,red)))

   ;; Syntax highlighting
   `(font-lock-builtin-face ((t (:foreground ,pink))))
   `(font-lock-comment-face ((t (:foreground ,fg-  :background ,bg :inherit italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg- :background ,bg :inherit italic))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-doc-face ((t (:foreground ,fg- :inherit italic))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-negation-face ((t (:foreground ,yellow))))
   `(font-lock-preprocessor-face ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,yellow))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:foreground ,red :background ,bg+))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t :foreground ,fg+)))
   `(rainbow-delimiters-depth-2-face ((t :foreground ,pink)))
   `(rainbow-delimiters-depth-3-face ((t :foreground ,purple)))
   `(rainbow-delimiters-depth-4-face ((t :foreground ,blue)))
   `(rainbow-delimiters-depth-5-face ((t :foreground ,cyan)))
   `(rainbow-delimiters-depth-6-face ((t :foreground ,green)))
   `(rainbow-delimiters-depth-7-face ((t :foreground ,yellow)))
   `(rainbow-delimiters-depth-8-face ((t :foreground ,orange)))
   `(rainbow-delimiters-unmatched-face ((t :foreground ,red :underline ,red)))

   ;; company
   `(company-tooltip ((t (:foreground ,fg :background ,bg :bold t))))
   `(company-tooltip-selection ((t (:foreground ,fg+ :background ,bg+ :bold t))))
   `(company-tooltip-mouse ((t (:foreground ,fg+ :background ,bg+ :bold t))))
   `(company-tooltip-common ((t (:foreground ,pink))))
   `(company-tooltip-common-selection ((t (:foreground ,purple))))
   `(company-tooltip-annotation ((t (:foreground ,orange))))
   `(company-preview ((t (:foreground ,fg :background ,bg :bold t))))
   `(company-preview-common ((t (:foreground ,fg+ :background ,bg+ :bold t))))
   `(company-scrollbar-bg ((t (:background ,bg))))
   `(company-scrollbar-fg ((t (:background ,fg))))
   `(company-template-field ((t (:foreground ,blue))))

   ;; diff-hl
   `(diff-hl-insert ((t :foreground ,green :background ,green)))
   `(diff-hl-change ((t :foreground ,orange :background ,orange)))
   `(diff-hl-delete ((t :foreground ,red, :background ,red)))

   ;; magit
   `(magit-branch ((t (:foreground ,cyan :weight bold))))
   `(magit-branch-local ((t (:foreground ,purple :weight bold))))
   `(magit-branch-remote ((t (:foreground ,yellow :weight bold))))
   `(magit-cherry-equivalent ((t (:foreground ,purple :weight bold))))
   `(magit-cherry-unmatched ((t (:foreground ,cyan :weight bold))))
   `(magit-diff-added ((t (:foreground ,green))))
   `(magit-diff-added-highlight ((t (:foreground ,green :background ,bg+))))
   `(magit-diff-context-highlight ((t (:background ,bg+))))
   `(magit-diff-file-header ((t (:foreground ,pink :box (:color ,pink)))))
   `(magit-diff-removed ((t (:foreground ,red))))
   `(magit-diff-removed-highlight ((t (:foreground ,red :background ,bg+))))
   `(magit-diffstat-added ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
   `(magit-hash ((t (:foreground ,pink))))
   `(magit-head ((t (:foreground ,purple))))
   `(magit-hunk-heading ((t (:foreground ,blue))))
   `(magit-hunk-heading-highlight ((t (:foreground ,blue :background ,bg+))))
   `(magit-item-highlight ((t (:foreground ,pink :background ,bg+))))
   `(magit-log-author ((t (:foreground ,cyan))))
   `(magit-log-graph ((t (:foreground ,fg-))))
   `(magit-log-head-label-bisect-bad ((t (:foreground ,red :box 1))))
   `(magit-log-head-label-bisect-good ((t (:foreground ,green :box 1))))
   `(magit-log-head-label-default ((t (:foreground ,fg+ :box 1))))
   `(magit-log-head-label-local ((t (:foreground ,purple :box 1))))
   `(magit-log-head-label-patches ((t (:foreground ,orange :box 1))))
   `(magit-log-head-label-remote ((t (:foreground ,yellow :box 1))))
   `(magit-log-head-label-tags ((t (:foreground ,green :box 1))))
   `(magit-log-sha1 ((t (:foreground ,pink :box 1))))
   `(magit-process-ng ((t (:foreground ,red :weight bold))))
   `(magit-process-ng ((t (:foreground ,red :weight bold))))
   `(magit-process-ok ((t (:foreground ,green :weight bold))))
   `(magit-process-ok ((t (:foreground ,green :weight bold))))
   `(magit-reflog-amend ((t (:foreground ,purple))))
   `(magit-reflog-checkout ((t (:foreground ,yellow))))
   `(magit-reflog-cherry-pick ((t (:foreground ,green))))
   `(magit-reflog-commit ((t (:foreground ,green))))
   `(magit-reflog-merge ((t (:foreground ,green))))
   `(magit-reflog-other ((t (:foreground ,cyan))))
   `(magit-reflog-rebase ((t (:foreground ,purple))))
   `(magit-reflog-remote ((t (:foreground ,cyan))))
   `(magit-reflog-reset ((t (:foreground ,red))))
   `(magit-section-heading ((t (:foreground ,yellow :weight bold))))
   `(magit-section-highlight ((t (:background ,bg+))))
   `(magit-section-title ((t (:foreground ,yellow :weight bold))))

   ;; whitespace
   `(whitespace-default ((t (:foreground ,fg-, :background ,bg))))
   `(whitespace-tab ((t (:foreground ,fg-- :inherit whitespace-default))))

   ;; RJSX mode
   `(rjsx-attr ((t :foreground ,orange :inherit italic)))
   `(rjsx-tag ((t :foreground ,red)))
   )

  (custom-theme-set-variables
   'nan
   `(ansi-color-names-vector
     [,bg ,red ,green ,orange ,blue ,pink ,cyan ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nan)
(provide 'nan-theme)

;;; nan-theme.el ends here
