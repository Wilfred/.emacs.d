(require 'magit)

;; Use F2 to open magit.
(global-set-key (kbd "<f2>") 'magit-status)

;; Don't prompt when first line of commit is over 50 chars.
(setq git-commit-finish-query-functions '())

;; When creating a new branch B from branch A, we don't want B to
;; track origin/A.
(setq magit-branch-arguments (remove "--track" magit-branch-arguments))

;; The default magit section highlighting is almost invisible on tangotango, so
;; use a darker grey (we're using the same as hl-line here).
;; TODO: send a patch to tangotango.
(custom-set-faces
 '(magit-section-highlight ((t (:background "grey14")))))

;; Highlight new/removed/changed lines relative to the last commit in
;; VCS.
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;; Set up keybindings for moving between changes in a file.
(global-set-key (kbd "C-.") #'diff-hl-next-hunk)
(global-set-key (kbd "C-,") #'diff-hl-previous-hunk)
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

;; `with-editor-finish' is bound to C-c C-c by default, which is a bit
;; tedious to type.
(define-key with-editor-mode-map (kbd "<f12>") #'with-editor-finish)

(defun wh/commit-and-push (prefix)
  (interactive "P")
  (add-hook 'with-editor-post-finish-hook
            (lambda ()
              (call-interactively #'magit-push-current-to-upstream))
            t t)
  (with-editor-finish nil))

(define-key with-editor-mode-map (kbd "<C-f12>") #'wh/commit-and-push)

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


(provide 'git-customisations)
