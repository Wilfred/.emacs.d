;;; Commentary
;; Completion is hard to get right. Currently, this is configured to
;; give several different types of completion. I want to port to
;; company-mode eventually, as it's more widely used (cf flycheck and
;; flymake).
;;
;; DUMB COMPLETION: This is often sufficient. Just complete the
;; current substring based on other strings in the buffer. If no
;; matches are present, try other buffers with the same mode. It must
;; be case sensitive.
;;
;; FULL LINE COMPLETION: It's often handy for imports if we can expand
;; the whole current line to match lines from other buffers (of the
;; same mode).
;;
;; FILENAME COMPLETION: If the current substring matches a filename of
;; a file at point, expand it to the full path. Occasionally useful.
;;
;; SYMBOL COMPLETION: In elisp or other languages with an inferior
;; process attached, complete symbols based on what is currently
;; defined. This should not affect buffers in other modes (a current
;; bug).
;;
;; LANGUAGE COMPLETION: Complete substrings that are known to exist in
;; a language. In the current configuration, I'm only doing this with
;; CSS.
;;
;; STATIC ANALYSIS COMPLETION: Complete methods and attributes on
;; classes. The current configuration only does this for Java with
;; eclim.

;; yasnippet, clever abbreviation expansion
(require 'yasnippet)

;; yasnippet includes too many snippets, only use the ones I have picked:
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; bind yas-expand to SPC, since TAB is used by ac-complete
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)

;; dabbrev-expand should match case
(require 'dabbrev)
(setq dabbrev-case-fold-search nil)

;; force hippie-expand completions to be case-sensitive
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)

(require 'dash)
(require 'hippie-exp)

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

;; fixme: we don't want lisp symbols on non-lisp modes
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
(global-set-key (kbd "M-/") 'hippie-expand)

;; auto-completion with neat popup
(require 'auto-complete-config)
(ac-config-default)
; using dabbrev as auto-completion source
(require 'ac-dabbrev)
(setq ac-sources
      (list ac-source-dabbrev))

;; don't try to complete after semicolon (is a pain in CSS)
(setq ac-ignores '(";"))

; always spaces, never tabs
(setq-default indent-tabs-mode nil)

;; Don't show 'AC' in the mode line.
(require 'diminish)
(diminish 'auto-complete-mode)

;; Always use 'y or n' for questions, since 'yes' is tedious to type over and over.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'completion-customisations)
