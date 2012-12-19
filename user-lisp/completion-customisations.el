; dabbrev-expand should match case
(setq dabbrev-case-fold-search nil)


(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-line-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; hippie-expand is over-eager but occasionally useful
;; so we bind it to M-? instead of M-/
(global-set-key (kbd "M-?") 'hippie-expand)


; auto-completion with neat popup
(add-to-list 'load-path "~/.emacs.d/third-party-lisp/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
; using dabbrev as auto-completion source
(require 'ac-dabbrev)
(setq ac-sources
      (list ac-source-dabbrev))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/third-party-lisp/auto-complete/dict")
(ac-config-default)

;; don't try to complete after semicolon (is a pain in CSS)
(setq ac-ignores '(";"))

; always spaces, never tabs
(setq-default indent-tabs-mode nil)

(provide 'completion-customisations)
