;;; js.el --- javascript related config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package json-mode)

(use-package js2-mode
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t))

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :config
  (add-to-list 'eglot-server-programs '(svelte-ts-mode . ("svelteserver" "--stdio")))
  (dolist (item svelte-ts-mode-language-source-alist)
    (add-to-list 'treesit-language-source-alist item))
  (add-to-list 'treesit-language-source-alist
               '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
                   "tsx/src"))))

;;; js.el ends here
