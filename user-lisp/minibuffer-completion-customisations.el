;;; Commentary:
;; Good minibuffer completion is crucial in Emacs: it affects user
;; efficiency and discoverability of Emacs features.
;;
;; My goals with configuring minibuffer completion are:
;;
;; * Efficient use of space. The minibuffer is rather small by
;; default. By using 'vertical completion' packages we can display
;; more candidates at once.
;;
;; * Effective narrowing. I should be able to type any substring (not
;; just the beginning) to find the values I'm looking for.

;;; ido:
;; For everyday usage, ido works really well. It's built-in, robust, reliable
;; and unsurprising.

;; The advantages of ido are:
;; * Simplicity. It doesn't open a new buffer like helm does.
;; * Convenient narrow. It may not be ergonomic, but I'm very familiar with it.
;; * Less invasive. Helm has opinions on how commands should work,
;;   whereas ido is a drop-in replacement.
;; * Low configuration. Smex just works, saving command history, whereas
;;   helm requires a separate package for this.
;; * Aesthetics. I prefer tools that stay in the minibuffer. Helm does
;;   not, and even defines its own font sizes, which I dislike.
;;
;; My customisations:
;; * Virtual buffers are nice for reopening files when switching buffers.
;; * Ordering
;;
;; The disadvantages:
;; * ido needs lots of packages. ido-ubiquitous does what ido-everywhere
;;   should have done. ido-vertical. smex.
;; * smex does not show keybindings (see my PR on the smex repo).
;; * slow for large datasets. Particularly C-h f and C-h v. There's
;;   ido-hacks (which forks parts of ido included in Emacs) and
;;   ido-flx (with slightly different behaviour).
;; 
;; Instead, we use ivy.

(use-package ivy
  :diminish ""
  :config
  ;; Enable ivy.
  (ivy-mode 1)

  ;; When switching buffers, offer recently accessed files that we don't
  ;; currently have open.
  (setq ivy-use-virtual-buffers t)

  ;; Don't require order, so 'func descr' matches 'describe-function'
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  ;; Don't show ./ and ../ when finding files with ivy.
  ;; To go up a directory, use backspace.
  (setq ivy-extra-directories nil)

  ;; Highlight the current selection with an arrow too.
  (setq ivy-format-function 'ivy-format-function-arrow)

  ;; Don't start the search term with ^ by default. I often have a
  ;; substring in mind.
  (setq ivy-initial-inputs-alist nil)

  ;; Use C-j for immediate termination with the current value, and RET
  ;; for continuing completion for that directory. This is the ido
  ;; behaviour for C-x C-f.
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  ;; Use C-RET as the same as RET, because I tend to accidentally press
  ;; C-RET.
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-alt-done)

  ;; Allow using the input as entered. This is useful when you want to
  ;; input a value that doesn't yet exist, such as creating a new file
  ;; with C-x C-f.
  (setq ivy-use-selectable-prompt t))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Use ido for projectile features, primarily C-x C-g (finding
;; files) and C-c p p (switching projects).
(require 'projectile)
(setq projectile-completion-system 'ivy)

(global-set-key (kbd "<f7>") #'counsel-imenu)

;; Always use 'y or n' for questions, since 'yes' is tedious to type over and over.
(fset 'yes-or-no-p 'y-or-n-p)

;; Save the contents of the minibuffer between emacs sessions.
(add-hook #'emacs-startup-hook #'savehist-mode)
(setq history-length 10000)

(provide 'minibuffer-completion-customisations)
