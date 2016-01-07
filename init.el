;; Generating This Document

;; init.org is the source file that I edit, and init.el and init.html are
;; generated from it. We define a convenience function to generate these files.


(defun wh/export-init ()
  "Generate init.html and init.html from the current init.org file."
  (interactive)
  (call-interactively #'org-babel-tangle)
  ;; Export as HTML 5, and include our styling overrides.
  (let ((org-html-doctype "html5")
        (org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"init.css\" />")
        (org-html-htmlize-output-type 'css))
    (call-interactively #'org-html-export-to-html)))

;; Folder Structure

;; Code I've written lives in user-lisp. This includes packages that I
;; haven't polished enough to release yet, small convenience functions,
;; and package customisations.

;; Interactive commands, configuration and keybindings are kept in files
;; named `FOO-customisations.el'. Elisp convenience functions are kept in
;; files named `FOO-utils.el'.


(add-to-list 'load-path "~/.emacs.d/user-lisp/")



;; Code that I haven't written lives in third-party-lisp. This directory
;; should ultimately disappear once all these packages live in [[http://melpa.milkbox.net/][MELPA]].


(add-to-list 'load-path "~/.emacs.d/third-party-lisp/")

;; Packages

;; I use ELPA packages heavily for functionality, primarily the MELPA
;; repository. We initalize all the packages here, so we can use them
;; later.
  

(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)



;; When a package is installed, it's byte-compiled. However, git is set
;; up to ignore .elc files (see the .gitignore file). The system that
;; installs the file therefore has .elc files, but other systems need to
;; byte-compile those directories.

;; To make matters worse, we can't just compile on startup any package
;; files that aren't compiled already, since some files fail compilation
;; every time. Instead, we compile directories that don't contain any
;; .elc files.


(require 'dash)
(require 'f)

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (--each (f-directories package-user-dir)
    (unless (was-compiled-p it)
      (byte-recompile-directory it 0))))

(ensure-packages-compiled)

;; todo: clean up orphaned .elc files

;; Paths

;; We set up Emacs' exec-path based on PATH in a shell. This is primarily
;; for OS X, where starting Emacs in GUI mode doesn't inherit the shell's
;; environment. This ensures that any command we can call from a shell,
;; we can call inside Emacs.

;; Note this function comes from the package `exec-path-from-shell.el'.
  

;; set exec-path according to the system's PATH
(exec-path-from-shell-initialize)

;; Theme

;; I like the tangotango theme. It has very contrasting colours and uses
;; bold faces for definitions. It also has good support for a range of
;; popular packages.

;; TODO: I don't like the large font sizes in org-mode. I would also
;; prefer zig-zag underlines for flycheck. There's also an issue with
;; hl-line or hl-sexp in the minibuffer with ido.


(load-theme 'tangotango t)

;; Visibility of UI Elements


;; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; show x-position (ie column number) for point in buffer
(column-number-mode 1)

;; Recursive Editing

;; We can make the minibuffer much more useful by enabling recursive
;; usage. This means that when the minibuffer is active we can still call
;; commands that require the minibuffer.


(setq enable-recursive-minibuffers t)


    
;; It's easy to lose track of whether we're in a recursive minibuffer or
;; not. We display the recursion level in the minibuffer to avoid confusion.


(minibuffer-depth-indicate-mode 1)

;; Moving Around

;; C-v and M-v don't undo each other, because the point position isn't
;; preservered. Fix that.


(setq scroll-preserve-screen-position 'always)

;; By Symbol

;; It's extremely useful to be able to move between different occurrences
;; of the same symbol.


(define-key prog-mode-map (kbd "M-n") #'highlight-symbol-next)
(define-key prog-mode-map (kbd "M-p") #'highlight-symbol-prev)

;; It's not clear to me why c++-mode-map isn't affected by prog-mode-map,
;; I suspect it's a bug (tested on 24.5).
(require 'cc-mode)
(define-key c++-mode-map (kbd "M-n") #'highlight-symbol-next)
(define-key c++-mode-map (kbd "M-p") #'highlight-symbol-prev)
(define-key c-mode-map (kbd "M-n") #'highlight-symbol-next)
(define-key c-mode-map (kbd "M-p") #'highlight-symbol-prev)
;; Java-mode has the same problem.
(define-key java-mode-map (kbd "M-n") #'highlight-symbol-next)
(define-key java-mode-map (kbd "M-p") #'highlight-symbol-prev)

;; Whilst YAML isn't a programming language, it's useful to move by
;; symbol here too.
(require 'yaml-mode)
(define-key yaml-mode-map (kbd "M-n") #'highlight-symbol-next)
(define-key yaml-mode-map (kbd "M-p") #'highlight-symbol-prev)



;; Jumping to the first occurrence of the symbol is handy for finding
;; where a symbol was imported.


(defun highlight-symbol-first ()
  "Jump to the first location of symbol at point."
  (interactive)
  (push-mark)
  (eval
   `(progn
      (goto-char (point-min))
      (search-forward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
       nil t)
      (beginning-of-thing 'symbol))))

(define-key prog-mode-map (kbd "M-P") #'highlight-symbol-first)



;; More rarely, it's useful to be able to jump to the last occurrence of
;; a symbol.


(defun highlight-symbol-last ()
  "Jump to the last location of symbol at point."
  (interactive)
  (push-mark)
  (eval
   `(progn
      (goto-char (point-max))
      (search-backward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
       nil t))))

(global-set-key (kbd "M-N") 'highlight-symbol-last)

;; By indentation

;; [[elisp:(describe-key%20(kbd%20"C-a"))][C-a]] normally moves us to the beginning of the line unconditionally
;; with [[elisp:(describe-function%20#'move-beginning-of-line)][move-beginning-of-line]]. This version is more useful, as it moves
;; to the first non-whitespace character if we're already at the
;; beginning of the line. Repeated use of `C-a' toggles between these two
;; positions.


(defun beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)
    
    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)

;; By Character

;; Vim has a handy command where you can type `f' to jump to the next
;; occurrence of a character on a line.

;; We can do this with `jump-char' without the constraint that the
;; character must be on the current line. This command needs to be
;; accessible with a short shortcut, so we use `M-m'. `M-m' is bound to
;; `back-to-indentation' by default, but our `C-a' behaviour makes it
;; redundant.


(require 'jump-char)

(global-set-key (kbd "M-m") #'jump-char-forward)
(global-set-key (kbd "M-M") #'jump-char-backward)

;; Measuring Movement

;; Since movement commands tend to be used more than any others, it's
;; useful to measure how much we use each command. This enables us to
;; look at frequent commands to see if we need to create custom commands
;; or different keybindings for common commands.


(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Inserting

;; It's often useful to start a new line of code that's above or below
;; the current line. This code is based on
;; http://emacsredux.com/blog/2013/03/26/smarter-open-line/ .


(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-o") 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  ;; TODO: edebug doesn't handle this error, suggesting that the
  ;; previous line throws the error. File a bug and/or find out why.
  (ignore-errors ; If we're at the beginning of the buffer.
    (newline-and-indent))
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-O") 'smart-open-line-above)

;; Killing

;; It's handy to also delete the trailing newline when using [[elisp:(describe-key%20(kbd%20"C-k"))][C-k]].


(defadvice kill-line (around kill-line-remove-newline activate)
  (let ((kill-whole-line t))
    ad-do-it))



;; I sometimes want to simply delete a region, rather than
;; saving it to the kill-ring. I've added a function that allows me to
;; type `C-u C-w' to delete the region, whilst `C-w' works as normal.


(defun kill-or-delete-region (beg end prefix)
  "Delete the region, storing it in the kill-ring.
If a prefix argument is given, don't change the kill-ring."
  (interactive "r\nP")
  (if prefix
      (delete-region beg end)
    (kill-region beg end)))

(global-set-key (kbd "C-w") 'kill-or-delete-region)

;; Matched Pairs

;; Smartparens is an excellent way of editing pairs of brackets, quotes
;; etc. It's similar to paredit, but can be used in lisp, other
;; programming languages and even HTML.

;; Currently, I only use a few smartparens commands, using the same
;; keybindings as the equivalent paredit commands. You can view a list of all smartparens
;; commands with the command `sp-cheat-sheet'.


(require 'smartparens)

;; (foo bar) -> foo bar
(define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)

;; (foo bar) -> [foo bar]
(define-key smartparens-mode-map (kbd "M-S") 'sp-rewrap-sexp)

;; (|foo) bar -> (|foo bar)
(define-key smartparens-mode-map (kbd "<C-right>") 'sp-slurp-hybrid-sexp)

;; (|foo bar) -> (|foo) bar
(define-key smartparens-mode-map (kbd "<C-left>") #'sp-forward-barf-sexp)

;; foo(1, |[2, 3], 4) -> foo(1, |, 2)
(define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "s-k") #'sp-kill-sexp)

(defun wh/smartparens-wrap-round (arg)
  "Smartparens equivalent of `paredit-wrap-round'."
  (interactive "P")
  (sp-wrap-with-pair "("))

(define-key smartparens-mode-map (kbd "M-(") #'wh/smartparens-wrap-round)

(defun wh/smartparens-wrap-singlequote (arg)
  "As `wh/smartparens-wrap-round' but for wrapping with single quotes."
  (interactive "P")
  (sp-wrap-with-pair "'"))

(define-key smartparens-mode-map (kbd "M-'") #'wh/smartparens-wrap-singlequote)



;; I like to use smartparens in all programming modes. Smartparens strict
;; mode ensures parens always stay balanced when editing. For example,
;; given code of the form =foo(1, |bar())=, C-k produces =foo(1, |)=.


(require 'smartparens-config)
(require 'smartparens-html)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)
(add-hook 'yaml-mode-hook #'smartparens-mode)

;; Opening

;; It's useful to be able to quickly open files that we opened before. We
;; define a function for this:


(require 'recentf)

;; offer recently accessed files from the menu
(recentf-mode t)

;; remember this many files
(setq recentf-max-saved-items 500)

;; from http://www.masteringemacs.org/article/find-files-faster-recent-files-package
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))



;; We bind this to `C-x C-r' (mnemonic: recent). By default, `C-x C-r' is bound to
;; `find-file-read-only', which isn't very useful. (You can set any file
;; as read only with `read-only-mode', mapped to `C-x C-q'.)


(global-set-key (kbd "C-x C-r") 'ido-recentf-open)



;; Most of the time though, it's helpful to be able to pick a file in the
;; same source code repository as the current buffer. There are several
;; tools to do this. I've played with `find-file-in-repository',
;; `projectile' and `find-file-in-project'.

;; `find-file-in-project' seems unmaintained. `find-file-in-repository'
;; is fast and works well, but is only lightly maintained and doesn't
;; support some version control systems. `projectile' is fast enough,
;; actively maintained and featureful.


(require 'projectile)
(projectile-global-mode)



;; We bind `projectile-find-file' to `C-x C-g', as we use it
;; a lot and it's right next to `C-x C-f'.


(global-set-key (kbd "C-x C-g") 'projectile-find-file)

;; Dired

;; Dired isn't very colourful by default, but `dired+' has helpful
;; highlighting.


(setq diredp-hide-details-initially-flag nil)
(require 'dired+)

;; Deleting and Backups

;; When we delete a file, it should go to the recycle bin rather than
;; just acting like shell:rm.


(setq delete-by-moving-to-trash t)



;; Emacs' backup behaviour is helpful, so we increase the number of
;; backups. However, rather than writing foo~1~ files everywhere, we
;; store all our backups in `~/.saves`.


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups



;; However, Emacs isn't aggressive enough with backups. We use
;; backup-each-save to ensure we have a copy of state of every file we've
;; modified.


(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; Scratch Files

;; It's often useful to create a throwaway file to write a minimal
;; testcase for some language or library feature.


(defun start--file (path)
  "Create a file at PATH, creating any containing directories as necessary.
Visit the file after creation."
  (make-directory (file-name-directory path) t)
  (find-file path))

(defun wh/start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (start--file (expand-file-name (format "~/scratch/%s" file-name))))

(defun wh/start-tmp-file (file-name)
  "Create a file in /tmp for the given file name."
  (interactive "sName of temporary file: ")
  (start--file (expand-file-name (format "/tmp/%s" file-name))))



;; It's also useful to quickly generate a minimal HTML page to play with.


(defun wh/start-scratch-html-file (file-name)
  "Create a test HTML file in ~/scratch to play around with."
  (interactive "sName of scratch HTML file: ")
  (wh/start-scratch-file file-name)
  (erase-buffer)
  (insert "<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <title>
        </title>
        <style type=\"text/css\">
        </style>
    </head>
    <body>

    </body>
</html>")
  (forward-line -2)
  (move-end-of-line nil))

;; Flymake

;; (Note that there's language-specific flymake configuration too.)

;; It's really useful to be able to move between flymake errors, so we
;; bind F8 and F9 for this. Since there's a gap between these two keys,
;; they're easy to find.


(require 'flymake)
(global-set-key (kbd "<f8>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f9>") 'flymake-goto-next-error)



;; When the cursor (point) is on a line, we want to show the error on
;; that line in the minibuffer.


(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)



;; I prefer my errors underlined.


(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

;; Flycheck

;; Flycheck is an excellent on-the-fly checker that provides many
;; additional features and languages. Flymake is part of stock Emacs,
;; flychcks is third-party.

;; Flycheck can be quite slow with a large number of errors. We reduce
;; how often we run it. We also change the highlighting to simply
;; highlight the whole line, as it's much faster. See
;; https://github.com/lunaryorn/flycheck/issues/153#issuecomment-19450255


(setq flycheck-highlighting-mode 'lines)



;; Style flycheck errors consistently with flymake.


(custom-set-faces
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange")))))



;; We use the same movement keys for flycheck as we do for flymake.


(require 'flycheck)
(define-key flycheck-mode-map (kbd "<f8>") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "<f9>") 'flycheck-next-error)



;; flycheck also provides a great overview buffer, but it's usually bound
;; to =C-c ! f=. This is tricky to type, so we use our own keybinding.


(define-key flycheck-mode-map (kbd "C-c f") #'flycheck-list-errors)



;; flycheck-next-error doesn't push the mark, so we can't use pop-mark to
;; go back to our previous position. We define and activate advice to fix
;; that.


(defadvice flycheck-next-error (around wh/flycheck-next-error-push-mark activate)
  (push-mark)
  ad-do-it)

;; Undoing

;; Emacs' undo facility is excellent, but undo-tree is even better.


(require 'undo-tree)
(global-undo-tree-mode)



;; Rather than just showing 'o' for edits, show a relative timestamp for
;; when the edit occurred.


(setq undo-tree-visualizer-timestamps t)



;; Since we're using it the whole time, it's not very informative to show
;; it on the mode line. Hide it.


(require 'diminish)
(diminish 'undo-tree-mode)

;; Shortcuts

;; =eval-defun= is bound to `C-M-x', but Gnome doesn't allow Emacs to
;; receive that key sequence. When writing elisp, it's very useful, so we
;; bind it to a convenient keybinding.

;; =edebug-eval-defun= is even more powerful. It ensures that =defvar=
;; and =defcustom= are re-evaluated, so they're reset to their initial
;; values. It can even mark a function for edebug, if it's called with a
;; prefix.


(require 'edebug)
(define-key emacs-lisp-mode-map (kbd "C-c e") #'edebug-eval-defun)



;; Similarly, toggle-debug-on-error is something I call a lot when
;; developing, and it doesn't have have any keybinding.


(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)



;; When writing and debugging macros, it's really important to be able
;; to see what they expand to. Macrostep allows us to incrementally
;; expand the macros in our elisp file.


(define-key emacs-lisp-mode-map (kbd "C-c m") 'macrostep-expand)

;; Editing Parentheses

;; Paredit make editing code with parentheses wonderful and has been the
;; gold standard for lisp coding for some time. Smartparens has recently
;; gained popularity as an paredit alternative, but I haven't invested
;; the time to set it up for lisp yet.


(add-hook 'emacs-lisp-mode-hook
          (lambda () (paredit-mode 1)))

;; Highlighting Parentheses

;; We colour each pair of parentheses according to their depth. This is
;; useful for seeing similarly nested lines, such as conditions in a
;; cond expression.


(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)



;; Our theme (tangotango) only provides colours for the first few nesting
;; levels before repeating. We override the face colours so we have
;; unique colours until we're seven levels deep.


(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "white")
(set-face-foreground 'rainbow-delimiters-depth-2-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
(set-face-foreground 'rainbow-delimiters-depth-4-face "green")
(set-face-foreground 'rainbow-delimiters-depth-5-face "orange")
(set-face-foreground 'rainbow-delimiters-depth-6-face "purple")
(set-face-foreground 'rainbow-delimiters-depth-7-face "white")
(set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
(set-face-foreground 'rainbow-delimiters-unmatched-face "red")

;; Function Signatures

;; We use eldoc to show the signature of the function at point in the
;; minibuffer.


(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)



;; We don't want this minor mode to be shown in the minibuffer, however.


(require 'diminish)
(require 'eldoc)
(diminish 'eldoc-mode)

;; On-the-fly Checking

;; It's really useful to use flycheck when coding elisp. It detects
;; mistyped variables, deprecated functions (everything that
;; byte-compilation checks).


(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)



;; By default, flycheck also runs checkdoc on elisp code. This gets in
;; the way for quick throwaway elisp scripts, so we switch off checkdoc.


(require 'flycheck)
(setq flycheck-checkers (--remove (eq it 'emacs-lisp-checkdoc) flycheck-checkers))

;; Highlighting

;; Emacs lisp highlighting works pretty well out of the box. However,
;; dash.el provides addition highlighting for its functions and variables
;; used in its anaphoric macros (e.g. `it').


(eval-after-load "dash" '(dash-enable-font-lock))

;; Python

;; We use pyflakes with flycheck to check for coding errors. Flycheck
;; includes other Python checkers so we also disable those.


(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'flycheck-disabled-checkers 'python-flake8)
            (add-to-list 'flycheck-disabled-checkers 'python-pylint)))



;; I like to write docstrings with example usage. These examples aren't
;; always valid doctests, so we switch off doctest checks.


(setenv "PYFLAKES_NODOCTEST" "y")



;; I often write triple-quoted docstrings, so it's convenient to have a
;; shortcut for inserting them.


(require 'python)

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\"\"\"")

(define-key python-mode-map (kbd "C-c s") 'python-insert-docstring)

;; Haskell

;; Flycheck supports Haskell well, so we switch it on inside Haskell
;; buffers.


(add-hook 'haskell-mode-hook 'flycheck-mode)



;; Tab doesn't indent in haskell-mode by default, so we enable
;; indentation.


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Ruby

;; Vagrant files are Ruby, so use Ruby syntax highlighting for them.


(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

;; C/C++

;; Flycheck supports C, so we switch it on.


(add-hook 'c-mode-common-hook #'flycheck-mode)



;; Always indent with 4 spaces, in the Linux kernel style.


(setq-default c-default-style "linux"
              c-basic-offset 4)



;; Hungry delete is useful in C (i.e. remove up to the next
;; non-whitespace character on C-d) when removing indentation.


(setq-default c-hungry-delete-key t)

;; HTML

;; I like to indent my HTML with tabs (company policy at the first web
;; shop I worked at).


(require 'sgml-mode)

; indent html with tabs only
(add-hook 'html-mode-hook
  (function
   (lambda ()
     (progn
       (setq indent-tabs-mode nil)
       (setq sgml-basic-offset 4)))))



;; Automatically close < and " character inside HTML using smartparens.


(require 'smartparens-config)
(add-hook 'html-mode-hook 'smartparens-mode)



;; Much of my HTML is for Django templates. These sometimes have .dtml
;; filenames, so use html-mode for those files.


(add-to-list 'auto-mode-alist '("\\.dtml$" . html-mode))



;; We want syntax highlighting for Django template syntax, so add extra
;; font faces and use them if we see Django syntax.


;; Define coloured faces for Django syntax.
(defvar django-tag-face (make-face 'django-tag-face))
(set-face-foreground 'django-tag-face "Orange")
;
(defvar django-variable-face (make-face 'django-variable-face))
(set-face-foreground 'django-variable-face "Green")

(defvar django-comment-face (make-face 'django-comment-face))
(set-face-foreground 'django-comment-face "Gray")

;; Use these faces for Django syntax.  
(font-lock-add-keywords
 'html-mode
 '(
   ("\\({%[^%]*%}\\)" 1 django-tag-face prepend)
   ("\\({{[^}]*}}\\)" 1 django-variable-face prepend)
   ("\\({#[^}]*#}\\)" 1 django-comment-face prepend)
   ("\\({% comment %}\\(.\\|
\\)*{% endcomment %}\\)" 1 django-comment-face prepend)
   ))



;; TODO: document the rest of our HTML configuration.


; skeletons for Django template tags
(define-skeleton template-tag-skeleton
  "Insert a {% foo %} template tag"
  "Template tag name: "
  "{% " str " %}")
(define-skeleton template-variable-skeleton
  "Insert a {{ foo }} template variable"
  "Template variable: "
  "{{ " str " }}")
(define-skeleton template-comment-skeleton
  "Insert a {# foo #} template variable"
  "Comment: "
  "{# " str " #}")
(define-skeleton template-block-skeleton
  "Insert {% block foo %}{% endblock %}"
  "Block name: "
  "{% block " str " %}\n" - "\n{% endblock %}")
(define-skeleton template-if-else-skeleton
  "Insert {% if foo %}{% else %}{% endif %}"
  "If condition: "
  "{% if " str " %}\n" - "\n{% else %}\n\n{% endif %}")
(define-skeleton template-if-skeleton
  "Insert {% if foo %}{% endif %}"
  "If condition: "
  "{% if " str " %}" - "{% endif %}")
(define-skeleton underscore-skeleton
  "Insert <%= foo %>"
  "Contents: "
  "<%= " str " %>")

(defvar template-skeletons
  '(template-tag-skeleton
    template-variable-skeleton
    template-comment-skeleton
    template-block-skeleton
    template-if-skeleton
    template-if-else-skeleton
    underscore-skeleton))

(defun insert-django-skeleton ()
  (interactive)
  (let* ((skeleton-names (mapcar 'symbol-name template-skeletons))
        (skeleton-chosen (ido-completing-read "HTML skeleton: " skeleton-names)))
    (funcall (intern skeleton-chosen))))

(define-key html-mode-map "\C-ct" 'insert-django-skeleton)

(defun visit-parent-django-template ()
  "In a buffer containg {% extends \"foo.html\" %}, visit foo.html."
  (interactive)
  (let (start-pos end-pos template-name)
    (save-excursion
      (widen)
      (goto-char (point-min))
      ;; Find the extends tag
      (while (not (looking-at "{% ?extends"))
        (forward-char 1))
      ;; Find the opening " of the file name.
      (while (not (looking-at "\""))
        (forward-char 1))
      (forward-char)
      (setq start-pos (point))

      ;; Find the closing "
      (while (not (looking-at "\""))
        (forward-char 1))
      (setq end-pos (point))

      (setq template-name (buffer-substring-no-properties start-pos end-pos)))

    ;; Open this file, assuming it's in the same directory.
    ;; TODO: Search the current VCS checkout for it.
    (find-file template-name)))

(defun html-linkify-region (url)
  "Wraps the region in an <a> tag with href set to URL."
  (interactive "sURL: ")
  (let* (
         (initial-cursor-position (point))
         (beginning (region-beginning))
         (end (region-end))
         (first-replacement (concat "<a href=\"" url "\">"))
         (second-replacement "</a>"))
  (goto-char beginning)
  (insert first-replacement)
  (goto-char (+ end (length first-replacement)))
  (insert second-replacement)
  (goto-char (+ initial-cursor-position (length first-replacement)))
  ))

; zen coding: converts selector-style lines to tags
; e.g. table>tr*2 becomes <table><tr></tr><tr></tr></table>
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; CSS

;; Typically I work on projects that use 4 spaces for CSS indenetation.


(add-hook 'css-mode-hook
          (function
           (lambda ()
             (progn
               (setq css-indent-offset 4)
               (setq indent-tabs-mode nil)))))


   
;; It's really handy to highlight CSS colour values to show the colour
;; they represent.


(add-hook 'css-mode-hook 'rainbow-mode)



;; Smartparens is well suited to CSS too, to automatically pair up curly
;; brackets.


(add-hook 'css-mode-hook 'smartparens-mode)

(add-hook 'css-mode-hook #'highlight-symbol-mode)



;; Company does a great job with completion for CSS, so use it here.


(add-hook 'css-mode-hook #'company-mode)

(defun wh/toggle-css-important ()
  (interactive)
  (save-excursion
    (end-of-line)
    (backward-char 1)
    (if (looking-back "!important")
        (delete-char (- (length " !important")))
      (insert " !important"))))

(require 'css-mode)
(define-key css-mode-map (kbd "C-c i") #'wh/toggle-css-important)

;; Less (CSS)

;; The less compiler doesn't give much feedback, but it does gives us a
;; syntax check.


(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'flymake-mode)

;; Org-mode

;; We often use code snippets in org-mode files, so syntax highlight
;; them.


(setq org-src-fontify-natively t)

;; Markdown

;; Markdown is essentially prose, so it's nice to automatically line-wrap
;; (by inserting newlines) as we type.


(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; Performance

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of
;; consing. The default value is 800,000 bytes, or ~ 0.7 MiB. By
;; increasing to 10 MiB we reduce the number of pauses due to garbage collection.


(setq gc-cons-threshold (* 10 1024 1024))

;; Shut Down

;; I rarely close Emacs, but using Zile means I use `C-x C-c' a lot. It's
;; annoying to accidentally close Emacs, so warn first.


(setq confirm-kill-emacs #'y-or-n-p)

;; Undocumented


(require 'ui-customisations)

(require 'file-customisations)
(require 'movement-customisations)
(require 'editing-customisations)
(require 'kill-ring-customisations)

(if (eq system-type 'darwin)
    (require 'os-x-fixes))

(require 'minibuffer-completion-customisations)

;; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;; treat space charcters as matching space characters, not like PCRE's '\s+'
(setq search-whitespace-regexp nil)

(require 'completion-customisations)
(require 'snippet-customisations)
(require 'structured-text-customisations)
(require 'isearch-customisations)

(require 'c-customisations)
(require 'asm-customisations)
(require 'coffee-customisations)
(require 'javascript-customisations)
(require 'lisp-customisations)
(require 'python-customisations)
(require 'rust-customisations)
(require 'llvm-customisations)
(require 'sh-customisations)
(require 'xml-customisations)

(require 'startup-customisations)

(require 'git-customisations)
(require 'eshell-customisations)

(require 'compilation-customisations)

(ignore-errors (require 'site-customisations))

(setq ag-highlight-search 't)
(global-set-key (kbd "<f5>") #'ag-project)

;; stolen from http://whattheemacsd.com/setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(require 'conflicts-customisations)

(require 'tags-utils)

(require 'blog-utils)

;; crontab mode for files named
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("crontab.*?\\'" . crontab-mode))

(setq ring-bell-function 'ignore)

;; suspend seems to crash on Gnome 3, and I don't use it anyway, so just disable it
(setq cannot-suspend t)
(defun suspend-emacs (&rest)
  (interactive))
(defun suspend-frame (&rest)
  (interactive))

;; windmove allows S-<right> and S-<right> to switch between windows
;; instead of `C-x o'
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-plugin-defalt-face ((t nil)))
 '(ethan-wspace-face ((t (:background "#2e3434"))))
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(highlight-symbol-face ((t (:underline t))))
 '(hl-line ((t (:background "gray14"))))
 '(hl-sexp-face ((t (:background "gray14"))))
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0))))
 '(smerge-refined-change ((t (:background "black")))))

(put 'narrow-to-region 'disabled nil)

(defun indent-buffer ()
  "Indent the everything in the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(require 'f)
(require 's)

(defun download-file (url directory file-name)
  "Download the file at URL into DIRECTORY.
The FILE-NAME defaults to the one used in the URL."
  (interactive
   ;; We're forced to let-bind url here since we access it before
   ;; interactive binds the function parameters.
   (let ((url (read-from-minibuffer "URL: ")))
     (list
      url
      (read-directory-name "Destination dir: ")
      ;; deliberately not using read-file-name since that inludes the directory
      (read-from-minibuffer
       "File name: "
       (car (last (s-split "/" url)))))))
  (let ((destination (f-join directory file-name)))
    (url-copy-file url destination 't)
    (find-file destination)))

(setq-default dired-listing-switches "-alhv")

(global-anzu-mode +1)

(require 'diminish)
(diminish 'anzu-mode)
(put 'dired-find-alternate-file 'disabled nil)

;; There are a few applications, such as crontab, that require a
;; trailing new line. To be safe, always leave a trailing newline.
(setq-default require-final-newline t)

;; cycle through amounts of spacing
;; http://pragmaticemacs.com/emacs/cycle-spacing/
(global-set-key (kbd "M-SPC") #'cycle-spacing)
