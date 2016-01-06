(require 'magit)

;; Use F2 to open magit.
(global-set-key (kbd "<f2>") 'magit-status)

;; Don't prompt when first line of commit is over 50 chars.
(setq git-commit-finish-query-functions '())

;; Disabling magit nagging when pushing.
(setq magit-push-always-verify nil)

;; When creating a new branch B from branch A, we don't want B to
;; track origin/A.
(setq magit-branch-arguments (remove "--track" magit-branch-arguments))

;; Highlight new/removed/changed lines relative to the last commit in
;; VCS.
(diff-hl-mode)
(diff-hl-flydiff-mode)

(global-set-key (kbd "C-.") #'diff-hl-next-hunk)
(global-set-key (kbd "C-,") #'diff-hl-previous-hunk)

;; Include 'x' in the magit popup.
;; From https://github.com/magit/magit/issues/2141
(magit-define-popup-action 'magit-dispatch-popup
  ?x "Reset" 'magit-reset ?!)

;; Default colours are too subtle, make them obvious.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(provide 'git-customisations)

