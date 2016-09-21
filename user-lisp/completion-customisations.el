;;; Commentary
;; There are two types of completion: precise, and greedy.
;;
;; Precise completion offers things like methods on classes, library
;; imports and CSS values. These are values that are always valid at
;; the current position.
;;
;; Greedy completion is the opposite: it offers variable names (even
;; in other scopes or files), filenames, anything that looks like it
;; might match the current prefix.
;;
;; Company excels at precise completion, but hippie-expand still has
;; the edge on greedy completion. Precise completion with company
;; requires language specific support, whereas hippie-expand works
;; very well with dabbrev in pretty much any language.

;; Greedy completion.

;; dabbrev-expand should match case
(require 'dabbrev)
(setq dabbrev-case-fold-search nil)
(require 'company)
(require 'company-dabbrev)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)
(require 'company-dabbrev-code)

;; Use company-dabbrev-code in all modes.
(setq company-dabbrev-code-modes t)

(global-set-key (kbd "s-/") #'company-dabbrev)

(global-set-key (kbd "C-z") #'company-try-hard)
(define-key company-active-map (kbd "C-z") #'company-try-hard)

;; Whole line completion is sufficiently useful that we give it a
;; separate keybinding.
(global-set-key (kbd "C-\\") #'company-whole-line)

;; Precise completion.

;; force hippie-expand completions to be case-sensitive
(defadvice hippie-expand (around hippie-expand-case-fold activate)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))

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

(use-package company
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local company-backends (list 'company-elisp))))
  :diminish "Comp")

(require 'company)
(require 'company-elisp)

(defadvice company-etags--candidates (around wh/etags-blacklist-modes activate)
  "Don't offer etags completion in modes where it's not helpful."
  (unless (memq major-mode '(asm-mode))
    ad-do-it))

(require 'company-files)
(require 'company-css)
(require 'company-clang)
(require 'company-tern)
(require 'company-anaconda)
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'prog-mode-hook #'company-mode)

;; Offer idle completion for three characters or more. (1 is very
;; noisy, and 2 hurts typing performance a little.)
(setq company-minimum-prefix-length 3)

;; Show a list of numbers next to completion options, where M-1
;; selects the first option and so on.
(setq company-show-numbers t)

;; In the completion list, wrap around so going backwards from the
;; last option shows the first.
(setq company-selection-wrap-around t)

;; Allow typing keys that don't match any candidates. This is useful
;; for imports, e.g. when we want to type foo::* in Rust but '*' isn't
;; in the candidates.
(setq company-require-match nil)

;; Align annotations to they're not shown immediately next to the
;; candidate. Otherwise, we end with a function foo shown as "foof".
(setq company-tooltip-align-annotations t)

;; Use C-n and C-p when company is active (for consistency with helm).
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(define-key company-active-map (kbd "RET") #'company-complete-selection)

;; Remember which completions we've used before, and sort those first.
(add-hook 'after-init-hook #'company-statistics-mode)

(require 'company-jit)
(add-hook 'python-mode-hook #'company-jit-mode)
(add-hook 'css-mode-hook #'company-jit-mode)

(require 'rust-mode)
(add-hook 'rust-mode-hook 'racer-mode)

(require 'company-whole-line)

;; always spaces, never tabs
(setq-default indent-tabs-mode nil)

;; TODO: Emacs is highlighting this incorrectly:
'other

(provide 'completion-customisations)
