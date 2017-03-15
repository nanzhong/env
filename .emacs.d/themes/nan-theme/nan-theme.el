;;; nan-theme.el --- Custom theme for nan -*- lexical-binding: t -*-

;; Copyright (C) 2017 Nan Zhong

;;; Commentary:
;; This is a personal theme that is heavily borrowed from the following themes:
;; - zerodark-theme

;;; Code:

(require 'all-the-icons)

(defmacro cached-for (secs &rest body)
  "Cache for SECS the result of the evaluation of BODY."
  (declare (debug t))
  (let ((cache (make-symbol "cache"))
        (last-run (make-symbol "last-run")))
    `(let (,cache ,last-run)
       (lambda ()
         (when (or (null ,last-run)
                   (> (- (time-to-seconds (current-time)) ,last-run)
                      ,secs))
           (setf ,cache (progn ,@body))
           (setf ,last-run (time-to-seconds (current-time))))
         ,cache))))

(deftheme nan
  "A custom theme for nan")

(defface nan-ro-face
  '((t :foreground "#0088CC" :weight bold))
  "Face for read-only buffer in the mode-line.")

(defface nan-modified-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Face for modified buffers in the mode-line.")

(defface nan-not-modified-face
  '((t :foreground "#98be65" :height 0.9))
  "Face for not modified buffers in the mode-line.")

(defface nan-buffer-position-face
  '((t :height 0.9))
  "Face for line/column numbers in the mode-line.")

(defface nan-vc-face
  '((t :foreground "#61afef"))
  "Face for vc status in the mode-line.")

(defface nan-ok-face
  '((t :foreground "#61afef"))
  "Face for ok status in the mode-line.")

(defface nan-warning-face
  '((t :foreground "#da8548"))
  "Face for warning status in the mode-line.")

(defface nan-error-face
  '((t :foreground "#ff6c6b"))
  "Face for error status in the mode-line.")

(defvar nan-modeline-position '(:eval (propertize ":%l:%c %p " 'face (if (nan--active-window-p)
									 'nan-buffer-position-face
								       'mode-line-inactive)))
  "Mode line construct for displaying the position in the buffer.")

(defvar nan-modeline-buffer-identification '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the position in the buffer.")

(defvar nan-modeline-modified '(:eval (if (buffer-modified-p (current-buffer))
					  (all-the-icons-faicon "floppy-o"
								:height 0.9
								:v-adjust 0
								:face (if (nan--active-window-p)
									  'nan-modified-face
									'mode-line-inactive))
					(all-the-icons-faicon "check"
							      :height 0.9
							      :v-adjust 0
							      :face (if (nan--active-window-p)
									'nan-not-modified-face
								      'mode-line-inactive)))))

(defvar nan-modeline-ro '(:eval (if buffer-read-only
				    (if (nan--active-window-p)
					(progn
					  (propertize "RO " 'face 'nan-ro-face))
				      (propertize "RO " 'face 'bold))
				  "")))

(defvar nan-buffer-coding '(:eval (unless (eq buffer-file-coding-system (default-value 'buffer-file-coding-system))
				    mode-line-mule-info)))

(defvar nan-modeline-vc '(vc-mode ("   "
				   (:eval (all-the-icons-faicon "code-fork"
								:height 0.9
								:v-adjust 0
								:face (when (nan--active-window-p)
									(nan-git-face))))
				   (:eval (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
						      'face (when (nan--active-window-p)
							      (nan-git-face)))))))

(defun nan-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when flycheck-mode
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (nan-face-when-active 'nan-error-face)))
                                (propertize "✔ No Issues"
                                            'face (nan-face-when-active 'nan-ok-face))))
                   (`running     (propertize "⟲ Running"
                                             'face (nan-face-when-active 'nan-warning-face)))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face (nan-face-when-active 'nan-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (nan-face-when-active 'nan-error-face)))
                   (`interrupted (propertize "⎊ Interrupted"
                                             'face (nan-face-when-active 'nan-error-face)))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defvar nan--git-face-cached (cached-for 1 (nan--git-face-intern)))

(defun nan--git-face-intern ()
  "Return the face to use based on the current repository status."
  (if (magit-git-success "diff" "--quiet")
      ;; nothing to commit because nothing changed
      (if (zerop (length (magit-git-string
                          "rev-list" (concat "origin/"
                                             (magit-get-current-branch)
                                             ".."
                                             (magit-get-current-branch)))))
          ;; nothing to push as well
          'nan-ok-face
        ;; nothing to commit, but some commits must be pushed
        'nan-warning-face)
    'nan-error-face))

(defun nan-git-face ()
  "Return the face to use based on the current repository status.
The result is cached for one second to avoid hiccups."
  (funcall nan--git-face-cached))


(let ((default "#abb2bf")
      (light "#ccd4e3")
      (background "#282c34")
      (background-dark "#24282f")
      (background-darker "#22252c")
      (mode-line-inactive "#1c2129")
      (mode-line-active "#6f337e")
      (background-lighter "#3a3f4b")
      (background-red "#4c3840")
      (bright-background-red "#744a5b")
      (background-purple "#48384c")
      (background-blue "#38394c")
      (bright-background-blue "#4e5079")
      (background-green "#3d4a41")
      (bright-background-green "#3f6d54")
      (background-orange "#4a473d")
      (hl-line "#2c323b")
      (grey "#cccccc")
      (grey-dark "#666666")
      (highlight "#3e4451")
      (comment "#687080")
      (orange "#da8548")
      (orange-light"#ddbd78")
      (red "#ff6c6b")
      (purple "#c678dd")
      (purple-dark "#64446d")
      (blue "#61afef")
      (blue-dark "#1f5582")
      (green "#98be65")
      (green-light "#9eac8c")
      (peach "PeachPuff3")
      (diff-added-background "#284437")
      (diff-added-refined-background "#1e8967")
      (diff-removed-background "#583333")
      (diff-removed-refined-background "#b33c49")
      (diff-current-background "#29457b")
      (diff-current-refined-background "#4174ae"))

  (custom-theme-set-faces
   'nan
   `(default ((t (:background ,background :foreground ,default))))
   `(cursor ((t (:background ,default))))

   ;; Highlighting faces
   `(fringe ((t (:background ,background :foreground ,comment))))
   `(border ((t (:foreground ,background-lighter))))
   `(vertical-border ((t (:foreground ,background-dark))))
   `(highlight ((t (:background ,highlight :foreground ,default :underline nil))))
   `(region ((t (:background ,highlight :foreground ,default))))
   `(secondary-selection ((t (:background ,highlight :foreground ,default))))
   `(isearch ((t (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((t (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((t (:background ,hl-line :underline unspecified :inherit nil))))

   `(match ((t (:background ,background-green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,blue :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   ;; `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,orange :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-doc-face ((t (:foreground ,green-light))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold :background ,background-red))))


   ;; Mode line faces
   `(mode-line ((t (:background ,background-dark :foreground ,light :box (:line-width 6 :color ,background-dark)))))
   `(mode-line-inactive ((t (:background ,background-dark :foreground ,comment :box (:line-width 6 :color ,background-dark)))))
   `(anzu-mode-line ((t :inherit mode-line :foreground ,purple :weight bold)))
   `(header-line ((t (:inherit mode-line-inactive))))

   ;; error & success
   `(error ((t (:foreground ,red :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(success ((t (:foreground ,green :weight bold))))

   ;; powerline
   `(powerline-active1 ((t (:height 0.9 :foreground ,blue :background ,background-darker))))
   `(powerline-active2 ((t (:height 0.9 :foreground ,blue :background ,background-lighter))))

   ;; mml
   `(message-mml-face ((t (:foreground ,comment))))

   ;; Org-clock mode line
   `(org-mode-line-clock ((t (:background unspecified (:inherit mode-line)))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((t (:foreground ,blue :weight bold))))
   `(escape-glyph ((t (:foreground ,blue :weight bold))))

   ;; linum
   `(linum ((t (:foreground ,comment :background ,background))))
   ;; from hlinum
   `(linum-highlight-face ((t (:foreground ,blue ,background ,background-blue))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,orange :background ,background :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,purple :background ,background :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,orange :background ,background :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,green :background ,background :weight bold))))

   ;; whitespace
   `(whitespace-space ((t (:background unspecified :foreground ,highlight
				       :inverse-video unspecified))))
   `(whitespace-hspace ((t (:background unspecified :foreground ,highlight
					:inverse-video unspecified))))
   `(whitespace-tab ((t (:background unspecified :foreground ,highlight
				     :inverse-video unspecified))))
   `(whitespace-newline ((t (:background unspecified :foreground ,highlight
					 :inverse-video unspecified))))
   `(whitespace-trailing ((t (:background ,red :foreground ,background :weight bold
					  :inverse-video nil))))
   `(whitespace-line ((t (:background unspecified :foreground ,red
				      :inverse-video unspecified))))
   `(whitespace-space-before-tab ((t (:inherit whitespace-space))))
   `(whitespace-space-after-tab ((t (:inherit whitespace-space))))
   `(whitespace-indentation ((t (:background unspecified :foreground ,highlight
					     :inverse-video unspecified))))
   `(whitespace-empty ((t (:background ,orange :foreground ,highlight
				       :inverse-video unspecified))))

   ;; link faces
   `(link ((t (:foreground ,blue :underline t))))
   `(link-visited ((t (:foreground ,blue :underline t))))

   ;; widget faces
   `(widget-field ((t (:background ,highlight :box (:line-width 1 :color ,comment)))))
   `(widget-button ((t (:inherit link))))

   ;; custom
   `(custom-button ((t (:background ,background-lighter :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((t (:background ,highlight :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((t (:background ,highlight :box (:line-width 2 :style pressed-button)))))
   `(custom-group-tag ((t (:foreground ,purple :weight bold :height 1.4))))
   `(custom-variable-tag ((t (:foreground ,purple :weight bold))))
   `(custom-state ((t (:foreground ,green))))

   ;; compilation
   `(compilation-info ((t (:foreground ,purple :weight bold))))
   `(compilation-warning ((t (:foreground ,orange :weight bold))))
   `(compilation-error ((t (:foreground ,red :weight bold))))
   `(compilation-line-number ((t (:foreground ,green :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,green :weight bold :inverse-video nil))))
   `(compilation-mode-line-run ((t (:foreground ,orange :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))

   ;; dired
   `(dired-header ((t (:foreground ,blue :background ,background-blue :weight bold))))
   `(dired-directory ((t (:foreground ,purple :weight bold))))

   ;; diff
   `(diff-removed ((t (:background ,background-red :foreground ,red))))
   `(diff-added ((t (:background ,background-green :foreground ,green))))
   `(diff-hunk-header ((t (:background ,background-blue :weight bold :foreground ,blue))))
   `(diff-file-header ((t (:weight bold))))
   `(diff-header ((t (:background ,background :foreground ,blue))))
   `(diff-context ((t (:foreground ,default))))
   `(diff-refine-added ((t (:foreground ,green :background ,bright-background-green))))
   `(diff-refine-removed ((t (:background ,bright-background-red :foreground ,red))))

   ;; ediff
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-fine-diff-C ((t (:foreground ,blue :background ,bright-background-blue))))
   `(ediff-current-diff-C ((t (:background ,background-blue :foreground ,blue))))

   ;; magit
   `(magit-diff-context-highlight ((t (:background ,background-darker))))
   `(magit-diff-file-heading ((t (:weight bold :foreground ,blue))))
   `(magit-diff-file-heading-highlight ((t (:weight bold :foreground ,blue :background ,background-blue))))
   `(magit-diff-removed-highlight ((t (:inherit diff-removed))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-added-highlight ((t (:inherit diff-added))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-lines-heading ((t (:background ,blue-dark :foreground "white"))))
   `(magit-diff-hunk-heading ((t (:background ,background-lighter))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,blue-dark))))
   `(magit-diff-hunk-heading ((t (:background ,background-lighter))))

   `(magit-process-ok ((t (:foreground ,green :weight bold))))

   `(magit-section-highlight ((t (:background ,background-darker))))
   `(magit-section-heading ((t (:foreground ,grey :weight bold))))
   `(magit-branch-current ((t (:foreground ,blue :background ,background-darker :box 1))))
   `(magit-branch-local ((t (:foreground ,purple :background ,background-darker :box 1))))
   `(magit-branch-remote ((t (:foreground ,green :background ,background-darker :box 1))))

   `(magit-reflog-reset ((t (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((t (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((t (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((t (:background ,background-green :foreground ,green :weight bold))))
   `(magit-reflog-checkout ((t (:background ,background-orange :foreground ,orange :weight bold))))
   `(magit-reflog-cherry-pick ((t (:background ,background-purple :foreground ,purple :weight bold))))

   `(magit-bisect-bad ((t (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((t (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-blame-heading ((t (:foreground ,green :background ,background-green :box 1))))

   `(git-commit-summary ((t (:weight bold))))

   `(magit-tag ((t (:foreground ,purple :weight bold :box 1 :background "#202020"))))
   `(magit-sequence-part ((t (:foreground ,orange :weight bold))))
   `(magit-sequence-head ((t (:foreground ,green :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:modified ((t (:foreground ,orange))))
   `(git-gutter-fr:added ((t (:foreground ,green))))
   `(git-gutter-fr:deleted ((t (:foreground ,red))))

   ;; Message faces
   `(message-header-name ((t (:foreground ,blue :weight bold))))
   `(message-header-cc ((t (:foreground ,purple))))
   `(message-header-other ((t (:foreground ,purple))))
   `(message-header-subject ((t (:foreground ,green))))
   `(message-header-to ((t (:foreground ,purple))))
   `(message-cited-text ((t (:foreground ,comment))))
   `(message-separator ((t (:foreground ,red :weight bold))))

   ;; ido faces
   `(ido-first-match ((t (:foreground ,purple :weight bold))))
   `(ido-only-match ((t (:foreground ,purple :weight bold))))
   `(ido-subdir ((t (:foreground ,blue))))

   ;; notmuch
   `(notmuch-message-summary-face ((t (:background ,highlight :box (:line-width 2 :color ,background)))))
   `(notmuch-search-count ((t (:foreground ,red :weight bold))))
   `(notmuch-search-matching-authors ((t (:foreground ,comment))))
   `(notmuch-search-subject ((t (:foreground ,default))))
   `(notmuch-search-unread-face ((t (:weight bold))))
   `(notmuch-search-date ((t (:foreground ,purple))))
   `(notmuch-crypto-part-header ((t (:foreground ,blue))))
   `(notmuch-crypto-decryption ((t (:foreground ,purple))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,red))))
   `(notmuch-crypto-signature-good ((t (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-good-key ((t (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-bad ((t (:background ,red :foreground ,background :weight bold))))
   `(notmuch-tag-face ((t (:foreground ,green :weight bold))))
   `(notmuch-tree-match-author-face ((t (:foreground ,purple))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,green :weight bold))))

   ;; company
   `(company-preview ((t (:background ,background-darker :foreground ,default))))
   `(company-preview-common ((t (:background ,background-darker :foreground ,purple))))
   `(company-preview-search ((t (:background ,blue :foreground ,default))))
   `(company-tooltip ((t (:background ,background-darker :foreground ,default))))
   `(company-scrollbar-bg ((t (:background ,background-darker))))
   `(company-scrollbar-fg ((t (:background ,background-blue))))
   `(company-tooltip-common ((t (:foreground ,purple :weight bold :background ,background-darker))))
   `(company-tooltip-annotation ((t (:foreground ,blue :weight bold :background ,background-blue))))
   `(company-tooltip-common-selection ((t (:foreground ,purple :background ,background-lighter :weight bold))))
   `(company-tooltip-selection ((t (:foreground ,default :background ,background-lighter))))
   `(company-tooltip-mouse ((t (:foreground ,default :background ,background-lighter))))

   ;; web-mode
   `(web-mode-html-tag-face ((t (:foreground ,purple :weight bold))))
   `(web-mode-symbol-face ((t (:foreground ,red :weight bold))))

   ;; js2-mode
   `(js2-function-param ((t (:foreground ,blue))))
   `(js2-error ((t (:foreground ,red))))

   ;; flycheck
   `(flycheck-fringe-error ((t (:foreground ,red :background ,background-red :weight bold :inverse-video t))))
   `(flycheck-fringe-warning ((t (:background ,background-orange :foreground ,orange :weight bold :inverse-video t))))
   `(flycheck-fringe-info ((t (:background ,background-blue :foreground ,blue :weight bold :inverse-video t))))
   `(flycheck-warning ((t (:underline (:color ,red :style wave)))))
   `(flycheck-error ((t (:underline (:color ,red :style wave)))))

   ;; FIC
   `(font-lock-fic-face ((t (:foreground ,background :background ,red :weight bold))))

   ;; org-mode todo
   `(org-hide ((t (:foreground ,background))))
   `(org-todo ((t (:foreground ,red :background ,background-red :weight bold))))
   `(org-done ((t (:foreground ,blue :background ,background-blue :weight bold))))
   `(org-date ((t (:background ,background-lighter))))
   `(org-scheduled-previously ((t (:foreground ,red))))
   `(org-scheduled ((t (:foreground ,default))))
   `(org-upcoming-deadline ((t (:foreground ,orange))))
   `(org-headline-done ((t (:foreground ,comment))))
   `(outline-1 ((t (:foreground ,blue :weight bold))))
   `(outline-2 ((t (:foreground ,purple :weight bold))))
   `(outline-3 ((t (:foreground ,peach :weight bold))))
   `(outline-4 ((t (:foreground ,green-light :weight bold))))
   `(outline-5 ((t (:foreground ,blue :weight bold))))
   `(outline-6 ((t (:foreground ,purple :weight bold))))
   `(outline-7 ((t (:foreground ,peach :weight bold))))
   `(outline-8 ((t (:foreground ,green-light :weight bold))))
   `(org-column-title ((t (:foreground unspecified :background unspecified))))
   `(org-agenda-date ((t (:foreground ,purple :weight bold))))
   `(org-agenda-date-today ((t (:foreground ,blue :weight bold :background ,background-blue :box 1))))
   `(org-agenda-structure ((t (:foreground ,blue :weight bold))))
   `(org-scheduled-today ((t (:foreground ,default :weight bold))))
   `(org-agenda-done ((t (:foreground ,comment))))
   `(org-time-grid ((t (:foreground ,comment))))

   ;; org columns
   `(org-column ((t (:background ,background-darker))))
   `(org-column-title ((t (:background ,background-blue :foreground ,blue :weight bold))))

   ;; org blocks
   `(org-block-begin-line ((t (:background ,background-green :foreground ,green-light :height 0.9))))
   `(org-block-end-line ((t (:background ,background-green :foreground ,green-light :height 0.9))))

   ;; Gnus faces -- from wombat, feel free to improve :)
   `(gnus-group-news-1 ((t (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((t (:foreground "#95e454"))))
   `(gnus-group-news-2 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((t (:foreground "#cae682"))))
   `(gnus-group-news-3 ((t (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((t (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((t (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((t (:foreground "#99968b"))))
   `(gnus-group-news-5 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((t (:foreground "#cae682"))))
   `(gnus-group-news-low ((t (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((t (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((t (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((t (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((t (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((t (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((t (:foreground "#99968b"))))
   `(gnus-header-content ((t (:foreground "#8ac6f2"))))
   `(gnus-header-from ((t (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((t (:foreground "#cae682"))))
   `(gnus-header-name ((t (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((t (:foreground "#cae682"))))

   ;; which-function
   `(which-func ((t (:foreground ,purple))))

   `(ediff-even-diff-A ((t (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-B ((t (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-C ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-A ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-B ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-C ((t (:background ,highlight :foreground unspecified))))

   ;; ivy
   `(ivy-current-match ((t (:background ,background-purple :weight bold :foreground ,purple))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,orange))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,green))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,orange))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,green))))
   `(ivy-match-required-face ((t (:foreground ,red :background ,background-red :weight bold))))
   `(ivy-modified-buffer ((t (:foreground ,red))))
   `(ivy-remote ((t (:foreground ,blue))))

   ;; helm
   `(helm-candidate-number ((t (:weight bold))))
   `(helm-header-line-left-margin ((t (:weight bold :foreground ,red))))
   `(helm-source-header ((t (:height 1.2 :weight bold :foreground ,blue :background ,background-blue))))
   `(helm-selection ((t (:background ,background-lighter))))
   `(helm-match ((t (:foreground ,purple :background ,background-purple :weight bold))))
   `(helm-match-item ((t (:inherit isearch))))
   `(helm-M-x-key ((t (:foreground ,blue :weight bold :background ,background-blue))))
   `(helm-visible-mark ((t (:weight bold :foreground ,orange :background ,background-darker))))
   `(helm-prefarg ((t (:weight bold :foreground ,red :background ,background-red))))
   `(helm-separator ((t (:weight bold :foreground , blue))))

   `(helm-grep-file ((t ())))
   `(helm-grep-finish ((t (:foreground ,green))))
   `(helm-grep-running ((t (:foreground ,red))))
   `(helm-grep-lineno ((t (:foreground ,blue))))
   `(helm-grep-match ((t (:foreground ,purple :background ,background-purple :weight bold))))

   `(helm-moccur-buffer ((t ())))

   `(helm-buffer-directory ((t (:foreground ,purple))))
   `(helm-buffer-file ((t ())))
   `(helm-buffer-process ((t (:foreground ,purple))))
   `(helm-buffer-size ((t (:foreground ,blue))))
   `(helm-buffer-saved-out ((t (:foreground ,red :weight bold))))

   `(helm-ff-directory ((t (:foreground ,purple))))
   `(helm-ff-dotted-directory ((t (:foreground ,purple))))
   `(helm-ff-prefix ((t (:weight bold :foreground ,red))))
   `(helm-ff-file ((t ())))
   `(helm-ff-executable ((t (:foreground ,green :weight bold :background ,background-green))))
   `(helm-ff-symlink ((t (:foreground ,orange))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red :weight bold :background ,background-red))))
   `(helm-history-deleted ((t (:foreground ,red :weight bold :background ,background-red))))

   ;; visible mark
   `(visible-mark-face1 ((t (:foreground ,orange-light :inverse-video t))))
   `(visible-mark-face2 ((t (:foreground ,peach :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((t (:background ,blue-dark))))

   ;; clojure
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))

   ;; ledger
   `(ledger-font-report-clickable-face ((t (:foreground ,blue))))
   `(ledger-font-posting-amount-face ((t (:foreground ,purple))))
   `(ledger-font-posting-date-face ((t (:foreground ,blue :background ,background-blue :box 1))))
   `(ledger-font-payee-uncleared-face ((t (:foreground ,default :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,green :weight bold))))
   `(ledger-font-posting-account-face ((t (:foreground ,default))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,red))))
   `(ledger-font-xact-highlight-face ((t (:background ,background-darker))))
   `(ledger-font-other-face ((t (:inherit ,font-lock-comment-face))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,orange))))

   `(diff-hl-change ((t (:foreground ,purple :background ,background-purple))))
   `(diff-hl-delete ((t (:foreground ,red :background ,background-red))))
   `(diff-hl-insert ((t (:foreground ,green :background ,background-green))))

   `(term-color-black ((t (:foreground ,default :background ,background-darker))))
   `(term-color-red ((t (:foreground ,red :background ,background-red))))
   `(term-color-green ((t (:foreground ,green :background ,background-green))))
   `(term-color-yellow ((t (:foreground ,orange :background ,background-orange))))
   `(term-color-blue ((t (:foreground ,blue :background ,background-blue))))
   `(term-color-magenta ((t (:foreground ,purple :background ,background-purple))))
   `(term-color-cyan ((t (:foreground ,blue-dark))))
   `(term-color-white ((t (:foreground ,grey))))
   `(term ((t (:foreground ,default :background ,background))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black))))

   `(sh-heredoc ((t (:foreground ,orange :weight bold))))

   `(avy-lead-face ((t :foreground ,red :background ,background-red)))
   `(avy-lead-face-0 ((t :foreground ,purple :background ,background-purple)))
   `(avy-lead-face-1 ((t :foreground ,blue :background ,background-blue)))
   `(avy-lead-face-2 ((t :foreground ,green :background ,background-green)))

   `(erc-nick-default-face ((t :foreground ,blue :background ,background-blue :weight bold)))
   `(erc-current-nick-face ((t :foreground ,red :weight bold :background ,background-red)))
   `(erc-my-nick-face ((t :foreground ,red :weight bold :background ,background-red)))
   `(erc-notice-face ((t :foreground ,comment)))
   `(erc-input-face ((t :foreground ,default :weight bold)))
   `(erc-prompt-face ((t :foreground ,purple :background ,background-purple :weight bold :box 1)))
   `(erc-timestamp-face ((t :foreground ,purple :weight bold)))

   `(hydra-face-red ((t :foreground ,red :weight bold)))
   `(hydra-face-blue ((t :foreground ,blue :weight bold)))

   ;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,blue))))
   `(elfeed-search-feed-face ((t (:foreground ,blue))))
   `(elfeed-search-tag-face ((t (:foreground ,green))))
   `(elfeed-search-title-face ((t (:foreground ,purple))))

   ;; wgrep
   `(wgrep-face ((t (:foreground ,orange))))
   `(wgrep-reject-face ((t (:foreground ,red :weight bold :background ,background-red))))
   `(wgrep-done-face ((t (:foreground ,blue :weight bold))))

   ;; AucTeX
   `(font-latex-math-face ((t :foreground ,green-light)))
   `(font-latex-sectioning-5-face ((t :foreground ,blue)))
   `(font-latex-string-face ((t :inherit font-lock-string-face)))
   `(font-latex-warning-face ((t :inherit warning)))

   ;; Anzu

   `(anzu-replace-highlight ((t :foreground ,red :background ,background-red :strike-through t)))
   `(anzu-replace-to ((t :foreground ,green :background ,background-green)))
   `(anzu-match-1 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-match-2 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-match-3 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-mode-line ((t :inherit mode-line :weight bold)))

   ;; jabber.el
   `(jabber-roster-user-online ((t :foreground ,blue :weight bold)))
   `(jabber-roster-user-error ((t :foreground ,red :background ,background-red :weight bold)))
   `(jabber-rare-time-face ((t :foreground ,comment)))
   `(jabber-chat-prompt-local ((t :foreground ,purple :background ,background-purple :weight bold)))
   `(jabber-chat-prompt-foreign ((t :foreground ,green :background ,background-green :weight bold)))
   `(jabber-activity-personal-face ((t :foreground ,red :background ,background-red :weight bold)))
   `(jabber-roster-user-away ((t :foreground ,orange)))
   `(jabber-roster-user-xa ((t :foreground ,orange)))

   ;; ace-window
   `(aw-leading-char-face ((t :foreground ,red :weight bold)))
   `(aw-background-face ((t :foreground ,comment)))

   ;; paren-face.el
   `(parenthesis ((t (:foreground ,comment))))

   ;; makefile
   `(makefile-space ((t (:background ,background-blue))))

   ;; epa
   `(epa-validity-high ((t (:foreground ,green))))
   `(epa-validity-low ((t (:foreground ,default))))
   `(epa-validity-disabled ((t (:foreground ,red :weight bold :background ,background-red))))
   `(epa-field-name ((t (:foreground ,purple :weight bold))))
   `(epa-field-body ((t (:foreground ,orange))))
   )

  (custom-theme-set-variables
   'nan
   `(ansi-color-names-vector [,background
                              ,red
                              ,green
                              ,orange
                              ,blue-dark
                              ,purple
                              ,blue
                              ,default])))

(defun nan-face-when-active (face)
  "Return FACE if the window is active."
  (when (nan--active-window-p)
    face))

;; So the mode-line can keep track of "the current window"
(defvar nan-selected-window nil
  "Selected window.")

(defun nan--set-selected-window (&rest _)
  "Set the selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq nan-selected-window window))))

(defun nan--active-window-p ()
  "Return non-nil if the current window is active."
  (eq (selected-window) nan-selected-window))

(add-hook 'window-configuration-change-hook #'nan--set-selected-window)
(add-hook 'focus-in-hook #'nan--set-selected-window)
(advice-add 'select-window :after #'nan--set-selected-window)
(advice-add 'select-frame  :after #'nan--set-selected-window)


;;;###autoload
(defun nan-setup-modeline-format ()
  "Setup the mode-line format for nan."
  (interactive)
  (require 'flycheck)
  (require 'magit)
  (setq-default mode-line-format
                `("%e"
                  " "
                  ,nan-modeline-ro " "
                  ,nan-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,nan-modeline-modified
                  " "
                  ,nan-modeline-buffer-identification
                  ,nan-modeline-position
                  ,nan-modeline-vc
                  "  "
                  (:eval (nan-modeline-flycheck-status))
                  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                  )))

(provide-theme 'nan)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; nan-theme.el ends here
