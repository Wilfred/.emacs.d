;; Use paredit for all the lisp modes I use.
(defun switch-on-paredit ()
  (turn-off-smartparens-mode)
  (smartparens-strict-mode -1)
  (paredit-mode 1))

(dolist (hook
         '(clojure-mode-hook
           emacs-lisp-mode-hook
           scheme-mode-hook
           lisp-mode-hook
           ielm-mode-hook))
  (add-hook hook 'switch-on-paredit))

;; Paredit's C-M-f and C-M-b are really handy but difficult to type.
(require 'paredit)
(define-key paredit-mode-map (kbd "s-f") #'paredit-forward)
(define-key paredit-mode-map (kbd "s-b") #'paredit-backward)
;; Likewise for other sexp commands.
(define-key paredit-mode-map (kbd "s-u") #'paredit-backward-up)
(define-key paredit-mode-map (kbd "s-d") #'paredit-forward-down)
(define-key paredit-mode-map (kbd "<s-backspace>") #'backward-kill-sexp)
(define-key paredit-mode-map (kbd "s-k") #'kill-sexp)
(define-key paredit-mode-map (kbd "s-t") #'transpose-sexps)
(define-key paredit-mode-map (kbd "s-n") #'paredit-forward-up)

;; Mark-sexp is also very useful but tricky to type.
(define-key paredit-mode-map (kbd "M-SPC") #'mark-sexp)

(use-package highlight-quoted
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'lisp-mode-hook #'highlight-quoted-mode))
;; Highlight the ' character itself in the same colour
;; as the quoted symbol.
(set-face-attribute 'highlight-quoted-quote nil
                    :inherit 'highlight-quoted-symbol)

(use-package nameless
  :config
  (setq nameless-private-prefix t)
  (define-key emacs-lisp-mode-map (kbd "_")
    #'nameless-insert-name-or-self-insert))

(dolist (hook '(emacs-lisp-mode-hook clojure-mode-hook))
  (add-hook hook
            (lambda ()
              (setq use-hl-line nil)
              (hl-sexp-mode))))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; rather than using TAGS, jump to function definitions that we have
;; loaded
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'diminish)
(diminish 'elisp-slime-nav-mode)

(add-hook 'ielm-mode-hook #'company-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(autoload 'clojure-mode "clojure-mode")

(require 'cider)

;; Trifle configuration
(require 'trifle-mode)

;; convenience function for Trifle indentation
;; todo: full-blown indentation functionality
(defun remove-indent ()
  (interactive)
  (back-to-indentation)
  (let ((indent-size (- (point) (line-beginning-position))))
    (delete-backward-char indent-size)))

;; Common Lisp configuration
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'slime)
(define-key slime-mode-map (kbd "C-c e") 'slime-eval-defun)

;; Ensure elisp code is continuously indented.
(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  :diminish "")

;; Use lispy-mode in emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)

(require 'lispy)

;; Disable the lispy keybindings that I don't want:
(define-key lispy-mode-map (kbd "M-.") nil)
(define-key lispy-mode-map (kbd "M-,") nil)
;; I use this keybinding a lot, to navigate between instances of a
;; variable with `highlight-symbol-next'.
(define-key lispy-mode-map (kbd "M-n") nil)
(define-key lispy-mode-map (kbd "M-p") nil)

;; lispy-underscore has some extra smarts, but they're only for
;; Clojure. I like using _ with `nameless-mode'.
(define-key lispy-mode-map (kbd "_") nil)

;; `lispy-move-beginning-of-line' adds nothing over
;; `move-beginning-of-line-dwim', but goes to column 1 first. I prefer
;; going back to indentation first, it's usually what I want.
(define-key lispy-mode-map-lispy (kbd "C-a") nil)

;; lispy-kill-at-point is really handy, but I use C-, for navigating
;; hunks.
(define-key lispy-mode-map-lispy (kbd "C-,") nil)

;; Work around https://github.com/abo-abo/lispy/issues/283
(remove-hook 'python-mode-hook #'wisent-python-default-setup)

(use-package lispy
  :config
  ;; Ensure pressing q closes edebug, macrostep and magit-blame.
  (setq lispy-compat '(edebug macrostep magit-blame-mode)))

(defun wh/elisp-imenu-reset ()
  (setq imenu-create-index-function #'imenu-default-create-index-function))

(add-hook 'emacs-lisp-mode-hook #'wh/elisp-imenu-reset)

(defun wh/split-pkg-version (pkg)
  "Split PKG into the package name and version.

E.g. \"~/.emacs.d/elpa/el-mock-20150906.321\" into \"el-mock\" and \"20150906.321\"."
  (message "pkg: %s" pkg)
  (let* ((filename (f-filename pkg))
         (parts (s-split (rx "-") filename))
         (version (-last-item parts))
         (package-name (s-join "-" (-butlast parts))))
    (list package-name version)))

(require 'f)

(defun wh/cleanup-old-elpa-dirs ()
  (interactive)
  (let* ((elpa-dirs (f-directories "~/.emacs.d/elpa"))
         ;; Ignore ~/.emacs.d/elpa/gnupg etc.
         (pkg-dirs (--filter (s-contains-p "-" it) elpa-dirs))
         ;; Todo: ensure '0.12' comes after '0.9'.
         (sorted-pkg-dirs (--sort (read (wh/split-pkg-version it)) pkg-dirs))
         (pkgs-by-version (--group-by
                           (-first-item (wh/split-pkg-version it)) sorted-pkg-dirs))
         (deleted-count 0))
    (--each pkgs-by-version
      (-let [(pkg . versions) it]
        (--each (-butlast versions)
          (f-delete it t)
          (incf deleted-count))))
    (message "Deleted %d directories in ~/.emacs.d/elpa" deleted-count)))

(require 'company)
(require 'company-elisp)

;; Use hungry-delete in elisp too.  There is also
;; `turn-on-hungry-delete-mode', but it seems that paredit's mode map
;; is coming first.
(define-key paredit-mode-map (kbd "C-d") 'hungry-delete-forward)

;; When evaluating lisp in the minibuffer, use paredit and eldoc.
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; from http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(autoload 'cider--make-result-overlay "cider-overlays")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(advice-add 'edebug-eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(setq lispy-eval-display-style 'overlay)

;; http://stackoverflow.com/a/17118588/509706
(defun wh/p (x)
  "Print VALUE to the current buffer.
Handy for ielm."
  (move-end-of-line 0)
  (insert (format "\n%s" x)))

(provide 'lisp-customisations)
