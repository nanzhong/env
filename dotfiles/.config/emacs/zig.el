;;; zig.el --- zig specific

;;; Commentary:

;;; Code:

(use-package zig-mode
  :elpaca (zig-mode :repo "~/src/github.com/nanzhong/zig-mode" :branch treesitter)
  :demand t
  :mode (("\\.zig\\'" . zig-ts-mode)
         ("\\.zon\\'" . zig-ts-mode))
  :init
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/maxxnino/tree-sitter-zig"))
  :config
  (setq-local treesit-font-lock-level 4))

;;; zig.el ends here
