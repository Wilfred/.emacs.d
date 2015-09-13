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

(defun wh/company-code ()
  "Generic completion in programming language buffers.
We just use symbols and lines from any open buffers in the current mode."
  (interactive)
  (let ((company-backends '((company-whole-line company-dabbrev-code))))
    (company-complete)))

;; yasnippet, clever abbreviation expansion
(require 'yasnippet)
(diminish 'yas-minor-mode)

;; yasnippet includes too many snippets, only use the ones I have picked:
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; bind yas-expand to SPC and not TAB.
;; TODO: this is still leaving TAB bound.
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
(require 'company)
(require 'company-dabbrev)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)
(require 'company-dabbrev-code)

;; Use company-dabbrev-code in all modes.
(setq company-dabbrev-code-modes t)

;; Although we only candidates to be taken from actual code, it's nice
;; to offer completion inside comments too.
(setq company-dabbrev-code-everywhere t)

;; Don't bother with company-dabbrev when we have company-dabbrev-code
;; already. It's a little too aggressive at suggesting words,
;; especially when writing comments.
(setq company-backends
      (remove 'company-dabbrev company-backends))

(global-set-key (kbd "C-z") #'company-try-hard)
(define-key company-active-map (kbd "C-z") #'company-try-hard)

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

;; Bind `company-complete' next to hippie-expand, because they're both useful.
(global-set-key (kbd "s-/") #'company-complete)

;; Use C-n and C-p when company is active (for consistency with helm).
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)

(require 'company-jit)
(require 'company-whole-line)

;; always spaces, never tabs
(setq-default indent-tabs-mode nil)

;; Always use 'y or n' for questions, since 'yes' is tedious to type over and over.
(fset 'yes-or-no-p 'y-or-n-p)

(use-package helm
  ;; TODO: find out why lazy-loading doesn't work here.
  :demand
  :init
  (setq helm-autoresize-min-height 50
        helm-autoresize-max-height 50
        helm-split-window-default-side 'below
        helm-split-window-in-side-p t)
  ;; Don't jump to the first definition when using helm-imenu.
  ;; See https://github.com/emacs-helm/helm/issues/1134
  ;; TODO: could we use `-remove-item' here? It's less verbose, but
  ;; we'd need to ensure dash is loaded.
  (setq helm-sources-using-default-as-input
        (remove 'helm-source-imenu helm-sources-using-default-as-input))

  ;; Don't use helm for C-x C-f. I prefer ido in this case (see
  ;; discussion at http://emacs.stackexchange.com/q/3798/304 ).
  (add-to-list 'helm-completing-read-handlers-alist '(find-file))

  ;; Helm increases the font size and uses a different font for its
  ;; header. Override that.
  (custom-set-faces
   '(helm-source-header ((t (:background "#22083397778B" :foreground "white")))))
  :bind
  ;; Ensure we show the shortest match when searching commands.
  ;; See http://emacs.stackexchange.com/q/10398/304
  ("M-x" . helm-M-x)
  :diminish helm-mode)

;; Buffer switching. I've experimented with `helm-mini' is nice, but
;; it doesn't sort buffers by recency. `helm-buffers-list' does sort
;; by recency, but sorts again as soon as you filter. See
;; https://github.com/emacs-helm/helm/issues/763 .

;; Use psession to preserve Emacs variables between sessions. We do
;; this so helm-M-x preserves command history between sessions. See
;; https://github.com/emacs-helm/helm/issues/431
(use-package psession
  :init
  ;; Don't bother preserving buffers.
  (setq psession-save-buffers-unwanted-buffers-regexp ".*")

  :config
  (psession-mode 1))

;; I'm seeing flickering/delayed drawing with `find-library' when
;; typing 'customisations'.
;;
;; I think the relevant commits may be
;; https://github.com/emacs-helm/helm/commit/9fe06d040ccd5234a9dae6b0d790c98e2ebedeee and
;; https://github.com/emacs-helm/helm/commit/7d107471406858f9ac3b17fd5eddf39accf193bf
;; relevant bug: https://github.com/emacs-helm/helm/issues/380

;; TODO: Emacs is highlighting this incorrectly:
'other

(provide 'completion-customisations)
