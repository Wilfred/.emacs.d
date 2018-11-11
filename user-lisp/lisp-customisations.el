;;; lisp-customisations --- Emacs configuration for lisp languages

(setq load-prefer-newer t)

(defun wh/trace-fn-at-point ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let* ((form (read (current-buffer)))
           (sym (nth 1 form)))
      (trace-function sym)
      (message "Tracing: %S" sym))))

(use-package elisp-mode
  :config
  (defun wh/run-ert-test ()
    "Run ERT test at point. If point is in a function, run the
test with the same name."
    (interactive)
    (save-excursion
      (beginning-of-defun)
      (let* ((form (read (current-buffer)))
             (sym (nth 1 form)))
        (ert sym))))
  (define-key emacs-lisp-mode-map (kbd "C-c r") #'wh/run-ert-test)

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-def-mode))

  ;; When a package is installed, it's byte-compiled. However, git is set
  ;; up to ignore .elc files (see the .gitignore file). The system that
  ;; installs the file therefore has .elc files, but other systems need to
  ;; byte-compile those directories.

  ;; To make matters worse, we can't just compile on startup any package
  ;; files that aren't compiled already, since some files fail compilation
  ;; every time. Instead, we compile directories that don't contain any
  ;; .elc files.

  (defun wh/was-compiled-p (path)
    "Does the directory at PATH contain any .elc files?"
    (--any-p (f-ext? it "elc") (f-files path)))

  ;; todo: clean up orphaned .elc files
  (defun wh/ensure-packages-compiled ()
    "If any packages installed with package.el aren't compiled yet, compile them."
    (interactive)
    (--each (f-directories package-user-dir)
      (unless (wh/was-compiled-p it)
        (byte-recompile-directory it 0)))))

(use-package elisp-def
  :diminish "")

;; I often end up looking up libraries.
(global-set-key (kbd "C-c l") #'counsel-find-library)


(use-package paredit
  :diminish ""
  :config
  ;; Paredit has a wealth of handy commands, but they're mostly bound
  ;; to C-M-something. This is awkward to type, especially in
  ;; environments where meta is intercepted (the standard Emacs work
  ;; around is to use ESC, so C-M-something becomes ESC C-something).
  ;;
  ;; Instead, for these sexp-oriented commands, we provide equivalent
  ;; keybindings using super (ie the windows key).
  ;;
  ;; Note that window managers tend to have windows key shortcuts, so
  ;; using the old C-M-something keybinding is still necessary
  ;; sometimes.
  (define-key paredit-mode-map (kbd "s-f") #'paredit-forward)
  (define-key paredit-mode-map (kbd "s-b") #'paredit-backward)
  ;; Likewise for other sexp commands.
  (define-key paredit-mode-map (kbd "s-u") #'paredit-backward-up)
  (define-key paredit-mode-map (kbd "s-d") #'paredit-forward-down)
  (define-key paredit-mode-map (kbd "s-n") #'paredit-forward-up)
  ;; For consistency, we do the same thing for other sexp-oriented
  ;; commands that ship with Emacs.
  (define-key paredit-mode-map (kbd "<s-backspace>") #'backward-kill-sexp)
  (define-key paredit-mode-map (kbd "s-t") #'transpose-sexps)
  (define-key paredit-mode-map (kbd "s-k") #'kill-sexp)

  ;; Use hungry-delete in elisp too.  There is also
  ;; `turn-on-hungry-delete-mode', but it seems that paredit's mode map
  ;; is coming first.
  (define-key paredit-mode-map (kbd "C-d") 'hungry-delete-forward)

  ;; Use paredit when evaluating lisp in the minibuffer.
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

  ;; We've configured smartparens to come on in all programming
  ;; modes. However, I find that paredit is more robust in lisps.
  (defun wh/switch-on-paredit ()
    "Enable paredit, and disable smartparens."
    (turn-off-smartparens-mode)
    (smartparens-strict-mode -1)
    (paredit-mode 1))
  (dolist (hook
           '(clojure-mode-hook
             emacs-lisp-mode-hook
             scheme-mode-hook
             racket-mode-hook
             lisp-mode-hook
             ielm-mode-hook))
    (add-hook hook #'wh/switch-on-paredit)))

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

(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                racket-mode-hook))
  (add-hook hook
            (lambda ()
              (setq use-hl-line nil)
              (hl-sexp-mode))))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(add-hook 'ielm-mode-hook #'company-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

(dolist (hook '(clojure-mode-hook
                scheme-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                racket-mode-hook))
  (add-hook hook #'rainbow-delimiters-mode))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook #'flycheck-mode))

(use-package geiser
  :config
  ;; Use racket-mode for racket.
  ;; https://github.com/jaor/geiser/pull/228
  (setq
   auto-mode-alist
   (remove '("\\.rkt\\'" . scheme-mode) auto-mode-alist)))

;; Common Lisp configuration
(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (define-key slime-mode-map (kbd "C-c e") 'slime-eval-defun))

;; Ensure elisp code is continuously indented.
(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  :diminish "")

(use-package lispy
  :config
  ;; Use lispy-mode in lisp languages.
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'scheme-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'racket-mode-hook #'lispy-mode)

  ;; Disable the lispy keybindings that I don't want:
  (define-key lispy-mode-map (kbd "M-.") nil)
  (define-key lispy-mode-map (kbd "M-,") nil)
  ;; I use this keybinding a lot, to navigate between instances of a
  ;; variable with `highlight-symbol-next'.
  (define-key lispy-mode-map (kbd "M-n") nil)
  (define-key lispy-mode-map (kbd "M-p") nil)

  ;; Lispy will insert a space before a colon, but I find this
  ;; annoying when contributing to projects that use : to delimit the
  ;; package namespace.
  (define-key lispy-mode-map (kbd ":") nil)
  (define-key lispy-mode-map-lispy (kbd ":") nil)

  ;; Use [ to insert a pair [] in lisp, because it's pretty common in
  ;; scheme and clojure. Keep ] as jumping to the close paren, as
  ;; that's useful and we autoclose square parens.
  (define-key lispy-mode-map-lispy (kbd "[") nil)

  ;; lispy-underscore has some extra smarts, but they're only for
  ;; Clojure. I like using _ with `nameless-mode'.
  (define-key lispy-mode-map (kbd "_") nil)

  ;; I find myself going up a lot, and M-/ is already convenient to
  ;; type. Use u for going up.
  ;; (define-key lispy-mode-map (kbd "u") #'paredit-backward-up)

  ;; `lispy-move-beginning-of-line' adds nothing over
  ;; `move-beginning-of-line-dwim', but goes to column 1 first. I prefer
  ;; going back to indentation first, it's usually what I want.
  (define-key lispy-mode-map-lispy (kbd "C-a") nil)

  ;; lispy-kill-at-point is really handy, but I use C-, for navigating
  ;; hunks.
  (define-key lispy-mode-map-lispy (kbd "C-,") nil)

  ;; Semantic mode can be switched on by lispy, which changes the
  ;; behaviour of a range of commands. Ensure it's only temporarily
  ;; switched on.
  (setq lispy-no-permanent-semantic t)

  ;; Ensure pressing q closes edebug, macrostep and magit-blame.
  (setq lispy-compat '(edebug macrostep magit-blame-mode)))

;; Work around https://github.com/abo-abo/lispy/issues/283
(remove-hook 'python-mode-hook #'wisent-python-default-setup)
(defun wh/elisp-imenu-reset ()
  (interactive)
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

;; When evaluating lisp in the minibuffer, use eldoc.
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

;; http://emacs.stackexchange.com/a/2302/304
(defun wh/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (shut-up
        (eval-defun nil)))))

;; Although this is a language agnostic tool, I only really use it in
;; elisp (and it works best there).
(global-set-key (kbd "C-c C-r") #'emr-show-refactor-menu)

;; Use helpful instead of Emacs' describe commands.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-c C-.") #'helpful-at-point)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Unfortunately, C-d C-d is used to terminate REPL sessions in
;; comint-mode, which means I often trigger it by accident in
;; ielm. Disable that.
(use-package comint
  :config
  (define-key comint-mode-map (kbd "C-c C-d") nil))

;; Since helpful exposes the manual anyway, use C-h F for looking up
;; functions.
(global-set-key (kbd "C-h F") #'helpful-function)

;; `describe-coding-system' isn't very useful, so lookup commands instead.
(global-set-key (kbd "C-h C") #'helpful-command)

(provide 'lisp-customisations)
