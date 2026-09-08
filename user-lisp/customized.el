;;; -*- lexical-binding: nil; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(ac-dabbrev ag aggressive-indent ansi anzu assess auto-yasnippet
                avy-zap backup-each-save bison-mode blacken
                brainfuck-mode browse-at-remote browse-kill-ring
                bug-hunter c-eldoc cask-mode cbm change-inner cider
                cmake-mode coffee-mode commander company
                company-anaconda company-c-headers company-quickhelp
                company-statistics company-tabnine company-tern
                company-try-hard counsel crontab-mode crux csv-mode
                cython-mode diff-hl dired+ disaster dockerfile-mode
                dpaste eglot el-mock el-x el2markdown elisp-def
                elisp-refs emacs-eclim emojify emr erlang
                ert-expectations eval-in-repl exec-path-from-shell
                ez-query-replace fancy-narrow feature-mode fic-mode
                firestarter flycheck-cask flycheck-pkg-config
                flycheck-pyflakes flycheck-rust flycheck-title ggtags
                gist git git-timemachine github-clone github-issues
                gitignore-mode go-mode google-maps groovy-mode
                hack-mode helm helm-ag helpful highlight-quoted
                highlight-symbol hl-sexp ht html-check-frag httprepl
                hungry-delete hyperbole ido-ubiquitous
                ido-vertical-mode imenu-anywhere impatient-mode
                interaction-log io-mode io-mode-inf jade-mode js2-mode
                js2-refactor json-mode jump-char keyfreq less-css-mode
                lispy list-environment litable llvm-mode logstash-conf
                lsp-mode lsp-treemacs lua-mode magit magit-section
                markdown-toc merlin merlin-eldoc modalka monky mosey
                move-dup multi-compile multifiles nameless nginx-mode
                nix-mode nodejs-repl org-bullets org-password-manager
                org-plus-contrib page-break-lines pcre2el php-mode
                pip-requirements pyfmt pyimport pytest python-django
                python-info racer rainbow-delimiters rainbow-mode
                rcirc-color realgud refine repl-toggle rhtml-mode
                scf-mode shut-up slime smalltalk-mode smart-tab
                smartparens smartscan smex sotlisp sqlplus string-edit
                suggest super-save sws-mode tabulated-list
                tangotango-theme toml-mode top-mode tuareg
                twittering-mode typescript-mode undercover undo-tree
                unfill use-package visual-regexp wgrep-ag
                whole-line-or-region yaml-mode yascroll zencoding-mode))
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `
           ((,(concat "("
                      (regexp-opt
                       '("sp-do-move-op" "sp-do-move-cl"
                         "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op"
                         "sp-do-del-cl")
                       t)
                      "\\_>")
             1 'font-lock-variable-name-face))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:weight bold))))
 '(ahs-plugin-defalt-face ((t nil)))
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4"))))
 '(eglot-semantic-declaration ((t (:inherit nil))))
 '(eglot-semantic-defaultLibrary ((t (:inherit nil))))
 '(eglot-semantic-function ((t (:inherit nil))))
 '(eglot-semantic-method ((t (:inherit nil))))
 '(eglot-semantic-operator ((t (:inherit nil))))
 '(eglot-semantic-property ((t (:inherit nil))))
 '(eglot-semantic-static ((t (:inherit nil))))
 '(ethan-wspace-face ((t (:background "#2e3434"))))
 '(fixed-pitch-serif ((t (:family "default"))))
 '(flymake-errline ((((class color)) (:underline "Red"))) t)
 '(flymake-warnline ((((class color)) (:underline "Orange"))) t)
 '(font-lock-fic-face ((t (:foreground "Red"))) t)
 '(highlight-symbol-face ((t (:underline t))))
 '(hl-line ((t (:background "gray14"))))
 '(hl-sexp-face ((t (:background "gray14"))))
 '(info-title-2 ((t (:inherit info-title-3))))
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline "Orange"))) t)
 '(magit-section-highlight ((t (:background "grey14"))))
 '(org-date ((((class color)) (:underline nil))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0))))
 '(smerge-refined-change ((t (:background "black"))) t))

