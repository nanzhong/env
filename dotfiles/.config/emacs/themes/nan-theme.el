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
      (bg-    "#01060E")
      (bg     "#0F1419")
      (bg+    "#141925")
      (bg++   "#232834")
      (bg+++  "#3D424D")

      (fg---  "#4e4e4e")
      (fg--   "#6c6c6c")
      (fg-    "#a6a6a6")
      (fg     "#E9E9E9")
      (fg+    "#FAFAFA")

      (white         "#E9E9E9")
      (bright-white  "#FFFFFF")
      (blue          "#59C2FF")
      (bright-blue   "#73D0FF")
      (cyan          "#4CBF99")
      (green         "#C2D94C")
      (bright-yellow "#FFD580")
      (yellow        "#FFB454")
      (orange        "#FA8D3E")
      (red           "#F0717B")
      (bright-red    "#F28779")
      (purple        "#A37ACC")
      (pink          "#ffa3aa")

      (vcs-addition      "#A6CC70")
      (vcs-modification  "#77A8D9")
      (vcs-removal       "#F27983"))

  (custom-theme-set-faces
   'nan

   ;; Default
   `(default ((t (:background ,bg :foreground ,fg))))
   `(italic ((t (:italic t))))
   `(cursor ((t (:background ,fg+))))
   `(ffap ((t :foreground ,fg+)))
   `(fringe ((t (:background ,bg))))
   `(highlight ((t (:background ,bg++))))
   `(line-number ((t :foreground ,fg---)))
   `(lazy-highlight ((t (:background ,orange))))
   `(link ((t (:foreground "#39BAE6" :underline t))))
   `(minibuffer-prompt ((t :foreground ,pink)))
   `(region ((t (:background "#33415E"))))
   `(show-paren-match-face ((t (:background ,red))))
   `(vertical-border ((t (:foreground ,bg+ :background ,bg+))))
   `(warning ((t (:foreground ,orange))))

   ;; tty-menu
   `(tty-menu-enabled-face ((t (:foreground ,fg :background ,bg+))))
   `(tty-menu-disabled-face ((t (:foreground ,fg- :background ,bg+))))
   `(tty-menu-selected-face ((t (:foreground ,fg+ :background ,bg++ :weight bold))))

   ;; modeline
   `(mode-line ((t (:background ,bg++ :foreground ,fg+ :box nil))))
   `(mode-line-inactive ((t (:background ,bg+ :foreground ,fg- :box nil))))

   ;; flymake
   `(flymake-note ((t :background "#5CCFE6" :foreground: ,bg)))
   `(flymake-warning ((t :background "#FFE6B3" :foreground: ,bg)))
   `(flymake-error ((t (:background "#FF3333" :foreground: ,fg))))

   ;; Syntax highlighting
   `(font-lock-builtin-face ((t (:foreground "#FFE6B3"))))
   `(font-lock-comment-face ((t (:foreground "#5C6773" :background ,bg :inherit italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#5C6773" :background ,bg :inherit italic))))
   `(font-lock-constant-face ((t (:foreground "#D4BFFF"))))
   `(font-lock-doc-face ((t (:foreground "#5C6773" :inherit italic))))
   `(font-lock-function-name-face ((t (:foreground "#5CCFE6"))))
   `(font-lock-keyword-face ((t (:foreground "#FFA759"))))
   `(font-lock-negation-face ((t (:foreground "#F28779"))))
   `(font-lock-preprocessor-face ((t (:foreground "#5C6773"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "#95E6CB"))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "#4CBF99"))))
   `(font-lock-string-face ((t (:foreground "#BAE67E"))))
   `(font-lock-type-face ((t (:foreground "#F28779"))))
   `(font-lock-variable-name-face ((t (:foreground "#73D0FF"))))
   `(font-lock-warning-face ((t (:foreground "#F28779"))))

   ;; lsp-ui
   `(lsp-ui-doc-background ((t (:background ,bg+))))
   `(lsp-ui-sideline-code-action ((t (:foreground "#C2D94C" :background ,bg+ :weight bold))))

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
   `(company-tooltip ((t (:foreground ,fg :background ,bg+ :bold t))))
   `(company-tooltip-selection ((t (:foreground ,fg+ :background ,bg++ :bold t))))
   `(company-tooltip-mouse ((t (:foreground ,fg+ :background ,bg+ :bold t))))
   `(company-tooltip-common ((t (:foreground ,pink))))
   `(company-tooltip-common-selection ((t (:foreground ,purple))))
   `(company-tooltip-annotation ((t (:foreground ,orange))))
   `(company-preview ((t (:foreground ,fg :background ,bg :bold t))))
   `(company-preview-common ((t (:foreground ,fg+ :background ,bg+ :bold t))))
   `(company-scrollbar-bg ((t (:background ,bg))))
   `(company-scrollbar-fg ((t (:background ,fg))))
   `(company-template-field ((t (:foreground ,blue))))

   ;; org-mode
   `(org-level-1 ((t (:foreground "#FFB454" :weight bold))))
   `(org-level-2 ((t (:foreground "#C2D94C" :weight bold))))
   `(org-level-3 ((t (:foreground "#95E6CB" :weight bold))))
   `(org-level-4 ((t (:foreground "#59C2FF" :weight bold))))
   `(org-level-5 ((t (:foreground "#F2AE49" :weight bold))))
   `(org-level-6 ((t (:foreground "#86B300" :weight bold))))
   `(org-level-7 ((t (:foreground "#4CBF99" :weight bold))))
   `(org-level-8 ((t (:foreground "#55B4D4" :weight bold))))

   `(org-todo ((t (:foreground "#77A8D9" :weight bold))))
   `(org-done ((t (:foreground "#A6CC70" :weight bold))))

   `(org-agenda-structure ((t (:foreground "#626A73"))))
   `(org-agenda-date-today ((t (:foreground ,fg+ :weight bold))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-scheduled-previously ((t (:foreground "#F27983"))))
   `(org-scheduled-today ((t (:foreground ,fg))))

   `(org-drawer ((t (:foreground "#5C6773"))))
   `(org-special-keyword ((t (:foreground "#D4BFFF" :weight bold))))
   `(org-tag ((t (:foreground "#FF9940" :weight bold))))

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
   `(magit-diff-added ((t (:foreground ,vcs-addition))))
   `(magit-diff-added-highlight ((t (:foreground ,vcs-addition :background ,bg+))))
   `(magit-diff-context-highlight ((t (:background ,bg+))))
   `(magit-diff-file-header ((t (:foreground ,pink :box (:color ,pink)))))
   `(magit-diff-removed ((t (:foreground ,vcs-removal))))
   `(magit-diff-removed-highlight ((t (:foreground ,vcs-removal :background ,bg+))))
   `(magit-diffstat-added ((t (:foreground ,vcs-addition))))
   `(magit-diffstat-removed ((t (:foreground ,vcs-removal))))
   `(magit-hash ((t (:foreground ,pink))))
   `(magit-head ((t (:foreground ,purple))))
   `(magit-hunk-heading ((t (:foreground ,blue))))
   `(magit-hunk-heading-highlight ((t (:foreground ,blue :background ,bg+))))
   `(magit-item-highlight ((t (:foreground ,pink :background ,bg+))))
   `(magit-log-author ((t (:foreground ,cyan))))
   `(magit-log-graph ((t (:foreground ,fg-))))
   `(magit-log-head-label-bisect-bad ((t (:foreground ,vcs-removal :box 1))))
   `(magit-log-head-label-bisect-good ((t (:foreground ,vcs-addition :box 1))))
   `(magit-log-head-label-default ((t (:foreground ,fg+ :box 1))))
   `(magit-log-head-label-local ((t (:foreground ,purple :box 1))))
   `(magit-log-head-label-patches ((t (:foreground ,orange :box 1))))
   `(magit-log-head-label-remote ((t (:foreground ,yellow :box 1))))
   `(magit-log-head-label-tags ((t (:foreground ,green :box 1))))
   `(magit-log-sha1 ((t (:foreground ,pink :box 1))))
   `(magit-process-ng ((t (:foreground ,vcs-removal :weight bold))))
   `(magit-process-ok ((t (:foreground ,vcs-addition :weight bold))))
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
   `(whitespace-line ((t (:foreground nil :background ,bg++))))

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
