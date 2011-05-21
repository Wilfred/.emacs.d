; Loading third party code
; ------------------------
;
; we will store all our 3rd party modes here
(add-to-list 'load-path "~/.emacs.d/user-lisp/")
; some plugins (at least w3) install themselves here:
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
; and unhelpfully, magit installed itself here
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(require 'ui-customisations)

; Interface
; ---------
;
;
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

; JavaScript
; ----------
;
; indent JavaScript tabs (treating them as eight spaces)
(setq-default js-indent-level 8)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)))

; better git handling
; note that this requires you to install magit manually, it's not in user-lisp
(require 'magit)

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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
; FIXME: breaks if you have another buffer called NEW-NAME
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

; something in the above has switched on debugging. Switch it off.
; TODO: find offending code
(setq debug-on-error nil)