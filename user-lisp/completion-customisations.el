; dabbrev-expand should match case
(setq dabbrev-case-fold-search nil)


; hippie-expand is overeage but occasionally useful
; so we bind it to M-? instead of M-/
(global-set-key (kbd "M-?") 'hippie-expand)


; auto-completion with neat popup
; using dabbrev as auto-completion source
(add-to-list 'load-path "~/.emacs.d/user-lisp/auto-complete")
(require 'ac-dabbrev)
(setq ac-sources
      (list ac-source-dabbrev))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/user-lisp/auto-complete/dict")
(ac-config-default)
;
; don't try to complete after semicolon (is a pain in CSS)
(setq ac-ignores '(";"))
; tab only for completion
(define-key ac-complete-mode-map "\r" nil)

; always spaces, never tabs
(setq-default indent-tabs-mode nil)
