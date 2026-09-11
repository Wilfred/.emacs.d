(use-package magit
  :config
  ;; The default magit section highlighting is almost invisible on tangotango, so
  ;; use a darker grey (we're using the same as hl-line here).
  ;; TODO: send a patch to tangotango.
  (custom-set-faces
   '(magit-section-highlight ((t (:background "grey14")))))

  ;; I never remember this command, so give it an alias. It's bound to C
  ;; in magit commit buffers.
  (defalias 'wh/magit-add-file-entry #'magit-commit-add-log))

;; Use F2 to open magit.
(global-set-key (kbd "<f2>") 'magit-status)

;; Don't prompt when first line of commit is over 50 chars.
(use-package git-commit
  :config
  (setq git-commit-finish-query-functions '()))

;; Copied from Tarsius' example in https://github.com/magit/magit/issues/3964
(defun wh/git-commit-co-authored-by (name mail)
  "Insert a header mentioning the person who co-authored the commit."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Co-authored-by" name mail))

;; Highlight new/removed/changed lines relative to the last commit in
;; VCS.
(diff-hl-flydiff-mode)

;; Set up keybindings for moving between changes in a file.
(global-set-key (kbd "C-c n") #'diff-hl-next-hunk)
(global-set-key (kbd "C-.") #'diff-hl-next-hunk)
(global-set-key (kbd "C-,") #'diff-hl-previous-hunk)
(global-set-key (kbd "C-c p") #'diff-hl-previous-hunk)
;; Remove the old keybindings, so smex suggests the keybindings above.
(define-key diff-hl-command-map (kbd "[") nil)
(define-key diff-hl-command-map (kbd "]") nil)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Magit has auto reverting of buffers. This is neat, but slow when
;; you have a lot of buffers open. Switching from a feature branch to
;; master, then pulling your merged branch ends up reverting all the
;; changes files twice1
(global-auto-revert-mode -1)

;; Default colours are too subtle, make them obvious.
(custom-set-faces
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4")))))

(defun wh/commit-and-push (prefix)
  (interactive "P")
  (add-hook 'with-editor-post-finish-hook
            (lambda ()
              (call-interactively #'magit-push-current-to-upstream))
            t t)
  (with-editor-finish nil))

(use-package with-editor
  :config
  (define-key with-editor-mode-map (kbd "<C-f12>") #'wh/commit-and-push))

(use-package git-gutter
  :diminish ""
  :config
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
  (global-set-key (kbd "C-c p") #'git-gutter:previous-hunk))

(provide 'git-customisations)
