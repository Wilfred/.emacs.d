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

;; auto-yasnippet is great for throwaway operations with redundant code.
(require 'auto-yasnippet)
(global-set-key (kbd "s-w") #'aya-create)
(global-set-key (kbd "s-y") #'aya-expand)

(provide 'snippet-customisations)
