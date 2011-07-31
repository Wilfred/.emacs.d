; Loading third party code
; ------------------------
;
; we will store all our 3rd party modes here
(add-to-list 'load-path "~/.emacs.d/user-lisp/")
; some plugins (at least w3) install themselves here:
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(require 'ui-customisations)
(require 'startup-customisations)

(require 'dired+)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))


; OS X fixes:
(defun insert-hash ()
  (interactive)
  (insert "#"))

(if (eq system-type 'darwin)
    (progn
      ; set Meta-3 to insert a # character
      (global-set-key "\263" 'insert-hash)
      ; same PATH as bash from Terminal
      (setenv
       "PATH"
       "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/bin/g4bin:/usr/local/git/bin:/usr/local/sbin:/usr/X11/bin:/Users/wilfredhughes/homebrew/bin:/Users/wilfredhughes/homebrew/Cellar/fish/1.23.1/bin:/usr/X11R6/bin")
      ; and add these to exec-path too, so Emacs can find git and so on
      (setq exec-path (append exec-path
                              (split-string (getenv "PATH") ":")))))

; Clipboard
; ---------
;
; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)
;
; show contents of kill ring on demand
(defun show-kill-ring ()
  "Show the contents of the kill ring in a pop-up"
  (interactive)
  (popup-menu 'yank-menu))
(global-set-key "\C-cy" 'show-kill-ring)
;
; Editing conveniences
; --------------------
;

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; Automatically indent the new line when we hit enter
(define-key global-map (kbd "RET") 'newline-and-indent)
;
; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode)
; zap-to-char but don't delete the character itself
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))
; bind it to the usual zap-to-char shortcut
(global-set-key "\M-z" 'zap-up-to-char)

(defun transpose-symbols (arg)
  "Interchange sybmols around point, leaving point at end of them.
With prefix arg ARG, effect is to take symbol before or around point
and drag it forward past ARG other symbol (backward if ARG negative).
If ARG is zero, the symbol around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-symbol arg))

; bind it to the usual transpose-word key combination
(global-set-key "\M-t" 'transpose-symbols)

; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;
; offer recent files
(require 'recentf)
;
; get rid of `find-file-read-only' and replace it with something
; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;
; enable recent files mode.
(recentf-mode t)
;
; 200 files ought to be enough.
(setq recentf-max-saved-items 200)
;
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;
; automatically finish quotes, brackets etc according to mode
(require 'autopair)
(autopair-global-mode)
;
; dabbrev-expand should match case
(setq dabbrev-case-fold-search nil)
;
; auto-completion with neat popup
; using dabbrev as auto-completion source
(add-to-list 'load-path "~/.emacs.d/user-lisp/auto-complete")
(require 'ac-dabbrev)
(setq ac-sources
      (list ac-source-dabbrev))
;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/user-lisp/auto-complete/dict")
(ac-config-default)
;
; don't try to complete after semicolon (is a pain in CSS)
(setq ac-ignores '(";"))
; tab only for completion
(define-key ac-complete-mode-map "\r" nil)

;
; always spaces, never tabs
(setq-default indent-tabs-mode nil)
;
; Text formatting
; ---------------
;
; csv mode stuff, since it's used extensively in GBBO
(require 'csv-mode)
; yaml mode stuff, since google app engine uses it
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'html-customisations)
(require 'css-customisations)
(require 'python-customisations)
(require 'javascript-customisations)

(require 'potato-customisations)

; better git handling
(add-to-list 'load-path "~/.emacs.d/user-lisp/magit")
(require 'magit)

(global-set-key [(f2)] 'magit-status)

(global-set-key [(f8)] 'flymake-goto-prev-error)
(global-set-key [(f9)] 'flymake-goto-next-error)

; clojure mode and other lisp necessities
(require 'clojure-mode)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

; deleting files should go to recycle bin
(setq delete-by-moving-to-trash t)

; note there is also set-visited-file-name but this is for name changes, not path changes
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (read-from-minibuffer "New name: " (buffer-name))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
          (rename-file name new-name 1)
          (rename-buffer new-name t)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))

; something in the above has switched on debugging. Switch it off.
; TODO: find offending code
(setq debug-on-error nil)

; suspend seems to crash on Gnome 3, and I don't use it anyway, so just remove the shortcut
(global-unset-key "\C-z")
