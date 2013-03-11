;; yasnippet, clever abbreviation expansion
(require 'yasnippet)

;; yasnippet includes too many snippets, only use the ones I have picked:
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; bind yas-expand to S-<TAB> (that's shift-tab), since TAB is used by ac-complete
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; dabbrev-expand should match case
(setq dabbrev-case-fold-search nil)

;; force hippie-expand completions to be case-sensitive
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)

(autoload '--filter "dash" nil t)
(autoload '--remove "dash" nil t)

;; only consider buffers in the same mode with try-expand-dabbrev-all-buffers
(defun try-expand-dabbrev-matching-buffers (old)
  (let ((matching-buffers (--filter
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun try-expand-dabbrev-other-buffers (old)
  (let ((matching-buffers (--remove
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-matching-buffers
        try-expand-dabbrev-other-buffers
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
(global-set-key (kbd "M-/") 'hippie-expand)


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
