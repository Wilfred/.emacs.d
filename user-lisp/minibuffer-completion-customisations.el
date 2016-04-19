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

;; reduce how often we get 'directory too big' problems:
(setq ido-max-directory-size 100000)

;; Show killed buffers at end when using ido for switching buffers.
(setq ido-use-virtual-buffers 'auto)

;; Don't switch to other directories if the current file/directory
;; doesn't exist. http://stackoverflow.com/a/7485815/509706
(setq ido-auto-merge-work-directories-length -1)

;; when using ido for opening files, show last modified first:
;; this version from http://jqian.googlecode.com/svn-history/r145/trunk/emacsconf/config/30-elisp.el
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)           ; avoid tramp
                (cond ((and (string-match "[:\\*]$" a) (not (string-match "[:\\*]$" b)))
                       nil)
                      ((and (string-match "[:\\*]$" b) (not (string-match "[:\\*]$" a)))
                       t)
                      ((and (string-match "[:\\*]$" a) (string-match "[:\\*]$" b))
                       nil)
                      (t
                       (let ((ta (nth 5 (file-attributes
                                         (concat ido-current-directory a))))
                             (tb (nth 5 (file-attributes
                                         (concat ido-current-directory b)))))
                         (cond ((and (null ta) tb) nil) ; avoid temporary buffers
                               ((and ta (null tb)) t)
                               ((and (null ta) (null tb)) nil)
                               (t (if (= (nth 0 ta) (nth 0 tb))
                                      (> (nth 1 ta) (nth 1 tb))
                                    (> (nth 0 ta) (nth 0 tb)))))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "When opening files, create their parent directories if they don't exist."
  (unless (f-exists? filename)
    (let ((dir (f-dirname filename)))
      (unless (f-exists? dir)
        (make-directory dir)))))

;; ido is nicer for finding files, due to the configuration above for sorting by recency, and
;; the ability to easily move into directories. See http://emacs.stackexchange.com/q/3798/304
(ido-mode t)

;; Use a vertical display for ido candidates. This shows more results,
;; and it's more attractive too.
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count nil)

;; Use ido wherever we can.
(ido-everywhere t)
(ido-ubiquitous-mode t)

;; Enable ido completion for etags.
(add-to-list 'ido-ubiquitous-command-overrides
             '(enable prefix "etags-select"))

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq org-completion-use-ido t)

;; Use ido for projectile features, primarily C-x C-g (finding
;; files) and C-c p p (switching projects).
(require 'projectile)
(setq projectile-completion-system 'ido)

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

(global-set-key (kbd "M-x") #'smex)

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
