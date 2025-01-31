((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id ace-window :type git
                       :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (apib-mode :source "elpaca-menu-lock-file" :recipe
            (:package "apib-mode" :fetcher github :repo
                      "w-vi/apib-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id apib-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c6dd05201f6eb9295736d8668a79a7510d11159e"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id async :type git :protocol https
                  :inherit t :depth treeless :ref
                  "5faab28916603bb324d9faba057021ce028ca847"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id avy :type git :protocol https
                :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id cfrs :type git :protocol https
                 :inherit t :depth treeless :ref
                 "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (clipetty :source "elpaca-menu-lock-file" :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id clipetty :type git :protocol
                     https :inherit t :depth treeless :ref
                     "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id cond-let :type git :protocol https
             :inherit t :depth treeless :ref
             "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id consult :type git :protocol
                    https :inherit t :depth treeless :ref
                    "080e2d6a5b20acf3f6c32ae0e1c88866853f3dfa"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github
                  :source "MELPA" :id corfu :type git :protocol https
                  :inherit t :depth treeless :ref
                  "20009d4fcc31770200b63a1440f15320ee009def"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :id dash
                 :type git :protocol https :inherit t :depth treeless
                 :ref "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id diff-hl :type git :protocol
                    https :inherit t :depth treeless :ref
                    "b965e19e6e7f9933199e421849a49229207c1c9f"))
 (direnv :source "elpaca-menu-lock-file" :recipe
         (:package "direnv" :fetcher github :repo
                   "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id direnv :type git :protocol
                   https :inherit t :depth treeless :ref
                   "c0bf3b81c7a97e2a0d06d05495e86848254fcc1f"))
 (dtrt-indent :source "elpaca-menu-lock-file" :recipe
              (:package "dtrt-indent" :fetcher github :repo
                        "jscheid/dtrt-indent" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id dtrt-indent :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "7c372bec8d84c247e4bd0d5599024d66ee300429"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo
                         "editorconfig/editorconfig-emacs" :old-names
                         (editorconfig-core editorconfig-fnmatch)
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id editorconfig :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4" :depth 1
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build (:not elpaca-build-docs) :source
                               "Elpaca extensions" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql"
                    :files (:defaults "README.md" "sqlite") :source
                    "MELPA" :id emacsql :type git :protocol https
                    :inherit t :depth treeless :ref
                    "2fe6d4562b32a170a750d5e80514fbb6b6694803"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :source "MELPA" :id embark :type git :protocol
                   https :inherit t :depth treeless :ref
                   "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark"
                           :fetcher github :files
                           ("embark-consult.el") :source "MELPA" :id
                           embark-consult :type git :protocol https
                           :inherit t :depth treeless :ref
                           "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "MELPA" :id
                                 exec-path-from-shell :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id f :type git :protocol https :inherit
              t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (fish-mode :source "elpaca-menu-lock-file" :recipe
            (:package "fish-mode" :fetcher github :repo
                      "wwwjfy/emacs-fish" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id fish-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id git-link :host github :type
                     git :protocol https :inherit t :depth treeless
                     :ref "d9b375f79e6071a9926bf73bba64111adfc93bf5"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode
                                          gitignore-mode)
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id git-modes :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (haml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "haml-mode" :repo "nex3/haml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id haml-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "3bb4a96535eb5c81dbe6a43bfa8d67a778d449c0"))
 (highlight-indent-guides :source "elpaca-menu-lock-file" :recipe
                          (:package "highlight-indent-guides" :fetcher
                                    github :repo
                                    "bumblepup/highlight-indent-guides"
                                    :files
                                    ("*.el" "*.el.in" "dir" "*.info"
                                     "*.texi" "*.texinfo" "doc/dir"
                                     "doc/*.info" "doc/*.texi"
                                     "doc/*.texinfo" "lisp/*.el"
                                     "docs/dir" "docs/*.info"
                                     "docs/*.texi" "docs/*.texinfo"
                                     (:exclude ".dir-locals.el"
                                               "test.el" "tests.el"
                                               "*-test.el"
                                               "*-tests.el" "LICENSE"
                                               "README*" "*-pkg.el"))
                                    :source "MELPA" :id
                                    highlight-indent-guides :type git
                                    :protocol https :inherit t :depth
                                    treeless :ref
                                    "802fb2eaf67ead730d7e3483b9a1e9639705f267"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id hl-todo :host github :type git
                    :protocol https :inherit t :depth treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ht :type git :protocol https
               :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
          (:package "htmlize" :fetcher github :repo
                    "emacsorphanage/htmlize" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id htmlize :type git :protocol
                    https :inherit t :depth treeless :ref
                    "fa644880699adea3770504f913e6dddbec90c076"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "MELPA" :id
                  hydra :type git :protocol https :inherit t :depth
                  treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
           (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id js2-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "e0c302872de4d26a9c1614fac8d6b94112b96307"))
 (json-mode :source "elpaca-menu-lock-file" :recipe
            (:package "json-mode" :fetcher github :repo
                      "json-emacs/json-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id json-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "466d5b563721bbeffac3f610aefaac15a39d90a9"))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id json-snatcher :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (kind-icon :source "elpaca-menu-lock-file" :recipe
            (:package "kind-icon" :repo
                      ("https://github.com/jdtsmith/kind-icon"
                       . "kind-icon")
                      :tar "0.2.2" :host gnu :files
                      ("*" (:exclude ".git")) :source "GNU ELPA" :id
                      kind-icon :type git :protocol https :inherit t
                      :depth treeless :ref
                      "556b0fb92aac24979b2c501431c7d48f75a5169f"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :id
                  llama :type git :protocol https :inherit t :depth
                  treeless :ref
                  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher
                     github :files
                     (:defaults (:exclude "init-tryout.el")) :source
                     "MELPA" :id lua-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "MELPA" :id lv :type git :protocol
               https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :id magit :host github :type git
                  :protocol https :inherit t :depth treeless :ref
                  "61950fca5e31c9a33d839b3994d93be6d370fb4c"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :id magit-section :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "61950fca5e31c9a33d839b3994d93be6d370fb4c"))
 (magit-todos :source "elpaca-menu-lock-file" :recipe
              (:package "magit-todos" :fetcher github :repo
                        "alphapapa/magit-todos" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-todos :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "7294a95580bddf7232f2d205efae312dc24c5f61"))
 (majutsu :source "elpaca-menu-lock-file" :recipe
          (:source nil :package "majutsu" :id majutsu :host github
                   :repo "0WD0/majutsu" :type git :protocol https
                   :inherit t :depth treeless :ref
                   "c329beb4a959efe2ad07007dc9c983a0dfbf34a3"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id marginalia :type git
                       :protocol https :inherit t :depth treeless :ref
                       "51a79bb82355d0ce0ee677151f041a3aba8cbfca"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id markdown-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "182640f79c3ed66f82f0419f130dffc173ee9464"))
 (multi-term :source "elpaca-menu-lock-file" :recipe
             (:package "multi-term" :fetcher github :repo
                       "manateelazycat/multi-term" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id multi-term :type git
                       :protocol https :inherit t :depth treeless :ref
                       "017c77c550115936860e2ea71b88e585371475d5"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo
                       "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source "MELPA" :id
                       nerd-icons :type git :protocol https :inherit t
                       :depth treeless :ref
                       "1db0b0b9203cf293b38ac278273efcfc3581a05f"))
 (nix-mode :source "elpaca-menu-lock-file" :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode"
                     :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :source "MELPA" :id nix-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id orderless :type git
                      :protocol https :inherit t :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (org :source "elpaca-menu-lock-file" :recipe
      (:package "org" :host github :repo "emacsmirror/org" :autoloads
                "org-loaddefs.el" :depth treeless :build
                ((:not elpaca-build-autoloads)
                 (:before elpaca-build-link elpaca-menu-org--build))
                :files
                (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
                :source "Org" :id org :type git :protocol https
                :inherit t :ref
                "6994fb7fdda89245095efba284e88dcd3d96c0d3"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id org-modern :type git
                       :protocol https :inherit t :depth treeless :ref
                       "713beb72aed4db43f8a10feed72136e931eb674a"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
           (:package "org-roam" :fetcher github :repo
                     "org-roam/org-roam" :files
                     (:defaults "extensions/*") :source "MELPA" :id
                     org-roam :type git :protocol https :inherit t
                     :depth treeless :ref
                     "7cd906b6f8b18a21766228f074aff24586770934"))
 (org-roam-ui :source "elpaca-menu-lock-file" :recipe
              (:package "org-roam-ui" :fetcher github :repo
                        "org-roam/org-roam-ui" :files
                        (:defaults "out") :source "MELPA" :id
                        org-roam-ui :type git :protocol https :inherit
                        t :depth treeless :ref
                        "2894dcbf56d2eca8d3cae2b1ae183f51724b5db6"))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
                   (:package "org-super-agenda" :fetcher github :repo
                             "alphapapa/org-super-agenda" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id org-super-agenda
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "fb20ad9c8a9705aa05d40751682beae2d094e0fe"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id pcre2el :type git :protocol
                    https :inherit t :depth treeless :ref
                    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture"
                    :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id pfuture :type git :protocol
                    https :inherit t :depth treeless :ref
                    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo
                     "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id posframe :type git :protocol
                     https :inherit t :depth treeless :ref
                     "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo
                          "protocolbuffers/protobuf" :files
                          ("editors/protobuf-mode.el") :source "MELPA"
                          :id protobuf-mode :type git :protocol https
                          :inherit t :depth treeless :ref
                          "230170c9f21b54bc945599f863cc452eec335f6c"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id rainbow-delimiters
                               :type git :protocol https :inherit t
                               :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :tar "1.0.6" :host gnu :branch
                         "externals/rainbow-mode" :files
                         ("*" (:exclude ".git")) :source "GNU ELPA"
                         :id rainbow-mode :type git :protocol https
                         :inherit t :depth treeless :ref
                         "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
              (:package "reformatter" :repo
                        "purcell/emacs-reformatter" :fetcher github
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id reformatter :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "c0ddac04b7b937ed56d6bf97e4bfcc4eccfa501a"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id s :type git :protocol https :inherit
              t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (simple-httpd :source "elpaca-menu-lock-file" :recipe
               (:package "simple-httpd" :repo
                         "skeeto/emacs-web-server" :fetcher github
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id simple-httpd :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (sly :source "elpaca-menu-lock-file" :recipe
      (:package "sly" :repo "joaotavora/sly" :fetcher github :files
                (:defaults "lib" "slynk" "contrib" "doc/images"
                           (:exclude "sly-autoloads.el"))
                :version-regexp "%v" :source "MELPA" :id sly :type git
                :protocol https :inherit t :depth treeless :ref
                "759c0ff8741ced8793257f2b7ed95a23e13e1407"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id smartparens :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (svelte-ts-mode :source "elpaca-menu-lock-file" :recipe
                 (:source nil :package "svelte-ts-mode" :id
                          svelte-ts-mode :host github :repo
                          "leafOfTree/svelte-ts-mode" :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "d079050fc1ba70f8fba9e596638daa2ca96e0fdd"))
 (svg-lib :source "elpaca-menu-lock-file" :recipe
          (:package "svg-lib" :repo
                    ("https://github.com/rougier/svg-lib" . "svg-lib")
                    :tar "0.3" :host gnu :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :id
                    svg-lib :type git :protocol https :inherit t
                    :depth treeless :ref
                    "925ed4a0215c197ba836e7810a93905b34bea777"))
 (swift-mode :source "elpaca-menu-lock-file" :recipe
             (:package "swift-mode" :repo "swift-emacs/swift-mode"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id swift-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "cfae3b85ad09bd293df941261afbc21e41bbb5f8"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo
                     "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons"
                                "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :source "MELPA" :id treemacs :type git :protocol
                     https :inherit t :depth treeless :ref
                     "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-icons-dired :source "elpaca-menu-lock-file" :recipe
                       (:package "treemacs-icons-dired" :fetcher
                                 github :repo
                                 "Alexander-Miller/treemacs" :files
                                 ("src/extra/treemacs-icons-dired.el")
                                 :source "MELPA" :id
                                 treemacs-icons-dired :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-magit :source "elpaca-menu-lock-file" :recipe
                 (:package "treemacs-magit" :fetcher github :repo
                           "Alexander-Miller/treemacs" :files
                           ("src/extra/treemacs-magit.el") :source
                           "MELPA" :id treemacs-magit :type git
                           :protocol https :inherit t :depth treeless
                           :ref
                           "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-nerd-icons :source "elpaca-menu-lock-file" :recipe
                      (:package "treemacs-nerd-icons" :fetcher github
                                :repo
                                "rainstormstudio/treemacs-nerd-icons"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :id
                                treemacs-nerd-icons :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "0c5ddcb978da639f01ddb023febc40fc755171e5"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ts :type git :protocol https
               :inherit t :depth treeless :ref
               "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "MELPA" :id vertico :type git
                    :protocol https :inherit t :depth treeless :ref
                    "f3c2033ba63880d6265cf1e1eb9e987792042fc4"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo
                  ("https://github.com/casouri/vundo" . "vundo") :tar
                  "2.4.0" :host gnu :files
                  ("*" (:exclude ".git" "test")) :source "GNU ELPA"
                  :id vundo :type git :protocol https :inherit t
                  :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id web-mode :type git :protocol
                     https :inherit t :depth treeless :ref
                     "e93b3fb89fd6345a5ff59795bed712abd486200a"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id websocket :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id which-key :type git
                      :protocol https :inherit t :depth treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id with-editor :type git :protocol https
             :inherit t :depth treeless :ref
             "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id yaml-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA" :id
                      yasnippet :type git :protocol https :inherit t
                      :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo
                               "AndreaCrotti/yasnippet-snippets"
                               :fetcher github :files
                               ("*.el" "snippets" ".nosearch") :source
                               "MELPA" :id yasnippet-snippets :type
                               git :protocol https :inherit t :depth
                               treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86"))
 (zig-mode :source "elpaca-menu-lock-file" :recipe
           (:package "zig-mode" :repo "nanzhong/zig-mode" :fetcher
                     codeberg :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id zig-mode :host github :branch
                     "tree-sitter" :type git :protocol https :inherit
                     t :depth treeless :ref
                     "31c0cc55dc46752faf226cc6493e10c26550cef1")))
