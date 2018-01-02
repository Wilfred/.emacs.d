(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (ggtags zencoding-mode yascroll yaml-mode whole-line-or-region wgrep-ag visual-regexp virtualenvwrapper use-package unfill undo-tree typescript-mode twittering-mode top-mode toml-mode tangotango-theme tabulated-list sws-mode super-save suggest string-edit sqlplus sotlisp smex smartscan smartparens smart-tab slime shut-up scf-mode rhtml-mode repl-toggle refine realgud rcirc-color rainbow-mode rainbow-delimiters racer python-info python-django pytest pyimport pyfmt pip-requirements php-mode pcre2el page-break-lines org-plus-contrib org-password-manager org-bullets nodejs-repl nginx-mode nameless multifiles multi-compile move-dup mosey modalka markdown-toc lua-mode logstash-conf llvm-mode litable list-environment lispy less-css-mode keyfreq jump-char json-mode js2-refactor jade-mode io-mode-inf io-mode interaction-log impatient-mode imenu-anywhere ido-vertical-mode ido-ubiquitous hyperbole hungry-delete httprepl html-check-frag hl-sexp highlight-symbol highlight-quoted groovy-mode google-maps go-mode gitignore-mode github-issues github-clone git-timemachine gist geiser flymake-jshint flycheck-title flycheck-rust flycheck-pyflakes flycheck-pkg-config flycheck-haskell flycheck-cask firestarter fic-mode feature-mode fancy-narrow ez-query-replace exec-path-from-shell eval-in-repl ert-expectations erlang emr emojify emacs-eclim elisp-refs el2markdown el-x el-mock dpaste dockerfile-mode disaster dired+ diff-hl cython-mode csv-mode crux crontab-mode counsel company-try-hard company-tern company-statistics company-quickhelp company-c-headers company-anaconda coffee-mode cmake-mode cider change-inner cbm cask-mode c-eldoc bug-hunter browse-kill-ring browse-at-remote brainfuck-mode bison-mode backup-each-save avy-zap auto-yasnippet assess anzu ansi aggressive-indent ag ac-dabbrev)))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))))))

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
 '(ethan-wspace-face ((t (:background "#2e3434"))))
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange"))))
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(font-lock-fic-face ((t (:foreground "Red"))) t)
 '(highlight-symbol-face ((t (:underline t))))
 '(hl-line ((t (:background "gray14"))))
 '(hl-sexp-face ((t (:background "gray14"))))
 '(info-title-2 ((t (:inherit info-title-3))))
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(magit-section-highlight ((t (:background "grey14"))))
 '(org-date ((((class color)) (:underline nil))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0))))
 '(smerge-refined-change ((t (:background "black"))) t))

