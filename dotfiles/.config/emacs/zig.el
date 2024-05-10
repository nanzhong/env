;;; zig.el --- zig specific

;;; Commentary:

;;; Code:

(use-package zig-mode
  :ensure (zig-mode :host github :repo "nanzhong/zig-mode" :branch "tree-sitter")
  :demand t
  :mode (("\\.zig\\'" . zig-ts-mode)
         ("\\.zon\\'" . zig-ts-mode))
  :init
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/maxxnino/tree-sitter-zig"))
  :hook (zig-ts-mode . (lambda ()
                         (setq treesit-font-lock-level 4))))

;;; zig.el ends here
