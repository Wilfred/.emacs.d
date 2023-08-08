(use-package magit
  :config
  ;; The default magit section highlighting is almost invisible on tangotango, so
  ;; use a darker grey (we're using the same as hl-line here).
  ;; TODO: send a patch to tangotango.
  (custom-set-faces
   '(magit-section-highlight ((t (:background "grey14")))))

  (defun wh/magit-branch-from-current-and-checkout (branch)
    "Create and checkout BRANCH from the current branch."
    (interactive (list (magit-read-string-ns "Branch name")))
    (let ((start-point (magit-get-current-branch)))
      (if (string-match-p "^stash@{[0-9]+}$" start-point)
          (magit-run-git "stash" "branch" branch start-point)
        (magit-call-git "checkout" "-b" branch start-point)
        (--when-let (and (magit-get-upstream-branch branch)
                         (magit-get-indirect-upstream-branch start-point))
          (magit-call-git "branch" (concat "--set-upstream-to=" it) branch))
        (magit-refresh))))

  (magit-define-popup-action 'magit-branch-popup
    ?f "new branch From current" #'wh/magit-branch-from-current-and-checkout)

  ;; I keep typing P (for push) instead of p. Set up an alias.
  (magit-define-popup-action 'magit-push-popup
    ?P "push alias" #'magit-push-current-to-upstream)

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
(global-diff-hl-mode)
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

;; Include 'x' in the magit popup.
;; From https://github.com/magit/magit/issues/2141
(magit-define-popup-action 'magit-dispatch-popup
  ?x "Reset" 'magit-reset ?!)

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
  (global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
  (global-set-key (kbd "C-c p") #'git-gutter:previous-hunk))

(provide 'git-customisations)
