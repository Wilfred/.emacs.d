;;; Commentary
;; Completion is hard to get right. I'm gradually moving to company,
;; but this is everything I expect from completion:
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
(diminish 'yas-minor-mode)

;; yasnippet includes too many snippets, only use the ones I have picked:
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; bind yas-expand to SPC and not TAB.
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)

(defun wh-yas-no-expand-in-comment/string ()
  "Don't expand yasnippets in strings or comments.
Taken from http://stackoverflow.com/a/25532190/509706."
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))
(add-hook 'prog-mode-hook #'wh-yas-no-expand-in-comment/string)

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

(require 'company)
(require 'company-dabbrev)
(require 'company-elisp)
(require 'company-files)
(require 'company-keywords) ;; might be annoying as many are short.
(require 'company-css)
(require 'company-clang)
(require 'company-tern)
(require 'company-anaconda)
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'prog-mode-hook #'company-mode)

(diminish 'company-mode)

;; Offer completion for as little as two characters (I've tried
;; setting this to one, but it's a bit noisy).
(setq company-minimum-prefix-length 2)

;; Show a list of numbers next to completion options, where M-1
;; selects the first option and so on.
(setq company-show-numbers t)

;; In the completion list, wrap around so going backwards from the
;; last option shows the first.
(setq company-selection-wrap-around t)

;; Select the current completion candidate when we press any character
;; from `company-auto-complete-chars'. This allows you to press space
;; or ; to carry on typing the relevant line of code.
(setq company-auto-complete t)

;; Bind `company-complete' next to hippie-expand, because they're both useful.
(global-set-key (kbd "s-/") #'company-complete)

;; Use C-n and C-p when company is active (for consistency with helm).
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(defun wh/company-complete-dwim ()
  "If we've just started completion, just complete the common prefix.
However, if called again, or if we've just selected a value,
select the current completion candidate."
  (interactive)
  (if (-contains? '(wh/company-complete-dwim
                    company-select-previous
                    company-select-next)
                  last-command)
      (company-complete-selection)
    (company-complete-common)))

(define-key company-active-map (kbd "<tab>") #'wh/company-complete-dwim)

;; always spaces, never tabs
(setq-default indent-tabs-mode nil)

;; Always use 'y or n' for questions, since 'yes' is tedious to type over and over.
(fset 'yes-or-no-p 'y-or-n-p)

;; helm
(require 'helm)
(setq helm-autoresize-min-height 50
      helm-autoresize-max-height 50
      helm-split-window-default-side 'below
      helm-split-window-in-side-p t)
;; Use helm wherever possible, e.g. for C-h f.
(helm-mode 1)

;; Don't bother showing helm in the mode line.
(diminish #'helm-mode)

;; Use helm for projectile features, primarily C-x C-g (finding
;; files) and C-c p p (switching projects).
(require 'projectile)
(setq projectile-completion-system 'helm)

;; TODO: Emacs is highlighting this incorrectly:
'other

(provide 'completion-customisations)
