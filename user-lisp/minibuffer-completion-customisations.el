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
;; For my everyday usage, ido works really well. It's robust, reliable
;; and unsurprising.

(require 'ido)

;; TODO: ido is a little slow for large option sets, e.g. C-h v or C-h
;; f. ido-flx looks hopeful as an alternative that's faster with
;; negligible change in experience. Ido-hacks also exists, but it's a
;; fork of ido.

;; The advantages of ido are:
;; * Simplicity. It doesn't open a new buffer like helm does.
;; * Convenient narrow. It may not be ergonomic, but I'm very familiar with it.
;; * Less invasive. Helm has opinions on how commands should work,
;; whereas ido is a drop-in replacement.
;; * Low configuration. Smex just works, saving command history, whereas
;; helm requires a separate package for this.
;; * Aesthetics. I prefer tools that stay in the minibuffer. Helm does
;; not, and even defines its own font sizes, which I dislike.
;;
;; My customisations:
;; * Virtual buffers are nice for reopening files when switching buffers.
;; * Ordering
;;
;; The disadvantages:
;; * ido needs lots of package. ido-ubiquitous does what ido-everywhere
;; should have done. ido-vertical. smex.
;; * smex does not show keybindings (see my PR on the smex repo).
;; * slow for large datasets. Particularly C-h f and C-h v.
;; 
;; Instead, we use ivy.
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Use ido for projectile features, primarily C-x C-g (finding
;; files) and C-c p p (switching projects).
(require 'projectile)
(setq projectile-completion-system 'ivy)

;;; helm:
;; Helm is an opinionated collection of great completion commands. I
;; don't like all the commands it offers, but some are very
;; worthwhile.
;;
;; There's a great overview of some of the things helm can do here:
;; http://tuhdo.github.io/helm-intro.html
;;
;; As a result, I don't use `helm-mode'. Instead, I use a handful of
;; helm commands that I really like, and I call them explicitly.

(use-package helm
  ;; TODO: find out why lazy-loading doesn't work here.
  :demand
  :init
  ;; Configure helm to open at the bottom of the Emacs frame, similar
  ;; to ido-vertical.
  (setq helm-autoresize-min-height 50
        helm-autoresize-max-height 50
        helm-split-window-default-side 'below
        helm-split-window-in-side-p t)
  ;; Don't jump to the first definition when using helm-imenu.
  ;; See https://github.com/emacs-helm/helm/issues/1134
  (require 'helm)
  (setq helm-sources-using-default-as-input
        (remove 'helm-source-imenu helm-sources-using-default-as-input))

  ;; Helm increases the font size and uses a different font for its
  ;; header. Override that.
  (custom-set-faces
   '(helm-source-header ((t (:background "#22083397778B" :foreground "white"))))))

(global-set-key (kbd "<f7>") #'helm-imenu)

;;; Known helm issues:
;; There are a number of commands where I tried helm, but wasn't happy
;; with the outcome so went back to ido.
;;
;; When quickly typing strings with `find-library', helm sometimes
;; pauses. I filed https://github.com/emacs-helm/helm/issues/380
;; but haven't managed to put together a reproducible test case.
;;
;; Buffer switching: I've experimented with `helm-mini', it's nice,
;; but it doesn't sort buffers by recency. `helm-buffers-list' does
;; sort by recency, but sorts again as soon as you filter. See
;; https://github.com/emacs-helm/helm/issues/763 .
;;
;; Opening files. Ido is extremely efficient for opening files, and
;; helm takes a different approach that takes some getting used
;; to. See the discussion in http://emacs.stackexchange.com/q/3798 .
;;
;; You're forced to use psession with helm to remember history for
;; `helm-M-x'. https://github.com/emacs-helm/helm/issues/431 I found
;; psession to interfere with my startup code (e.g. the startup quote) and didn't investigate
;; further.
;;
;; When helm-mode is active, M-x isn't bound to helm-M-x. You get helm
;; completion, but it's not as good:
;; http://emacs.stackexchange.com/questions/10398/getting-helm-to-default-to-the-shortest-match

;;; Helm features I love:
;; `helm-M-x' shows both matching commands and matching commands in
;; history. It also shows the keybindings for commands.
;;
;; `helm-imenu' shows additional information and is more attractive.

;; Always use 'y or n' for questions, since 'yes' is tedious to type over and over.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'minibuffer-completion-customisations)
