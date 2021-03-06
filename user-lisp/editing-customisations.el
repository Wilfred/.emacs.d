;; Fastest load, according to https://github.com/jwiegley/use-package#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;; Editing customisations -- general text munging when typing.
(require 's)
(require 'dash)
(require 'f)

;; highlight region whenever mark is active
(transient-mark-mode t)

;; overwrite when text is selected
(delete-selection-mode t)

;; Automatically indent the new line when we hit enter
(define-key global-map (kbd "RET") 'newline-and-indent)

;; zap-to-char but don't delete the character itself bind it to the
;; usual zap-to-char shortcut. With a prefix, prompt for the char to
;; zap up to.
(require 'avy-zap)
(setq avy-zap-dwim-prefer-avy nil) ;; https://github.com/cute-jumper/avy-zap/issues/3

(setq avy-zap-forward-only t)

(defadvice avy-zap-up-to-char-dwim (around wh/any-direction-when-avy activate)
  "When calling with a prefix argument, allow zapping in any direction.
Only zap forwards otherwise."
  (if current-prefix-arg
      (let ((avy-zap-forward-only nil))
        ad-do-it)
    ad-do-it))

(defadvice zap-up-to-char (around zap-case-sensitive activate)
  "Ensure `zap-up-to-char' is case sensitive.
This command is generally called from `avy-zap-up-to-char-dwim'."
  (let ((case-fold-search nil))
    ad-do-it))

(global-set-key (kbd "M-z") #'avy-zap-up-to-char-dwim)

;; to be consistent with C-M-f as forward-sexp, bind C-M-backspace to backward-kill-sexp
;; and C-M-d to forward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

(defun wh/toggle-case-next-char ()
  "Toggles the case of the next character after point.
The point is also moved one character forward."
  (interactive)
  ;; note next-char is a string
  (let ((next-char (buffer-substring (point) (1+ (point))))
        (case-fold-search nil)) ; case sensitive
    (if (string-match "[a-z]" next-char)
        (upcase-region (point) (1+ (point)))
      (downcase-region (point) (1+ (point)))))
  (forward-char))

;; toggling on char is more useful than capitalising a whole word since
;; it doesn't break camelcase
(global-set-key (kbd "M-c") #'wh/toggle-case-next-char)

(autoload 're-find-all "regexp-utils")
(autoload 're-match-p "regexp-utils")

(defun wh/format-symbol (string format)
  "Convert a given string to a specified formatting convention.

 \(wh/format-symbol \"fooBar\" 'constant) => \"FOO_BAR\""
  (let ((components))
    ;; split the string into its word components
    (if (s-contains? "_" string)
        (setq components (split-string string "_"))
      (setq components (re-find-all "[A-Z]?[a-z]+" string)))
    ;; format each component as a lowercase string
    (setq components (mapcar 'downcase components))
    (cond
     ((eq format 'constant) (mapconcat 'upcase components "_"))
     ((eq format 'camelcase) (mapconcat #'wh/toggle-case-first-char components ""))
     ((eq format 'camelcase-lower) (wh/toggle-case-first-char
                                    (mapconcat #'wh/toggle-case-first-char components "")))
     ((eq format 'variable-underscore) (mapconcat (lambda (x) x) components "_"))
     ((eq format 'variable-hyphen) (mapconcat (lambda (x) x) components "-"))
     (t (error "Unknown symbol format")))))

(defun wh/toggle-case-first-char (string)
  (let ((first-char (substring string 0 1))
        (rest (substring string 1)))
    (if (re-match-p "[a-z]" first-char)
        (setq first-char (upcase first-char))
      (setq first-char (downcase first-char)))
    (concat first-char rest)))

(defun wh/cycle-symbol-case ()
  "Convert toggle symbol at mark between the forms \"fooBar\",
\"FooBar\", \"FOO_BAR\" and \"foo_bar\"."
  (interactive)
  (let* ((symbol (symbol-name (symbol-at-point)))
         (symbol-bounds (bounds-of-thing-at-point 'symbol))
         (bound-start (car symbol-bounds))
         (bound-end (cdr symbol-bounds))
         (case-fold-search nil))
    (when symbol-bounds
      (goto-char bound-start)
      (kill-forward-chars (- bound-end bound-start))
      (cond
       ((re-match-p "[a-z-]+$" symbol) (insert (wh/format-symbol symbol 'variable-underscore)))
       ((re-match-p "[a-z_]+$" symbol) (insert (wh/format-symbol symbol 'camelcase-lower)))
       ((re-match-p "[a-z]+" symbol) (insert (wh/format-symbol symbol 'camelcase)))
       ((re-match-p "[A-Z][a-z]+" symbol) (insert (wh/format-symbol symbol 'constant)))
       (t (insert (wh/format-symbol symbol 'variable-underscore)))))))

(global-set-key (kbd "C-M-c") 'wh/cycle-symbol-case)


(defun wh/transpose-symbols (arg)
  "Interchange sybmols around point, leaving point at end of them.
With prefix arg ARG, effect is to take symbol before or around point
and drag it forward past ARG other symbol (backward if ARG negative).
If ARG is zero, the symbol around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-symbol arg))

;; bind it to the usual transpose-word key combination
(global-set-key (kbd "M-t") #'wh/transpose-symbols)

;; based on my-increment-number-decimal from http://www.emacswiki.org/emacs/IncrementNumber
;; unlike that version, we only change the number if point is on a number
(defun wh/increment-number-decimal (&optional arg)
  "Increment the number forward from point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (looking-at "[0-9]+")
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun wh/decrement-number-decimal (&optional arg)
  (interactive "p*")
  (wh/increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "<C-up>") #'wh/increment-number-decimal)
(global-set-key (kbd "<C-down>") #'wh/decrement-number-decimal)

;; don't use shift to set the mark, or caps lock creates regions the whole time
(setq-default shift-select-mode nil)

;; use expand-region to grow the region according to the current mode's syntax
(use-package expand-region
  :config
  (require 'python-el-fgallina-expansions)
  (global-set-key (kbd "C-=") 'er/expand-region))

(global-move-dup-mode)
(diminish 'move-dup-mode)

(global-set-key (kbd "C-c o") #'md/duplicate-down)

(use-package ez-query-replace
  :bind (("M-%" . ez-query-replace)
         ("C-c M-%" . ez-query-replace-repeat)))

;; open line and indent is a frequent and useful operation
;; so we can go from this (where `|` represents the cursor):
;;    foo("bar", {|})
;; to this:
;;    foo("bar", {
;;        |
;;    })
(defun open-newline-and-indent ()
  (interactive)
  ;; Move any text after point to the next line, and indent it.
  (newline-and-indent)

  ;; Move back to the end of the starting line.
  (forward-line -1)
  (end-of-line)

  ;; Start a correctly indented blank line.
  (newline-and-indent))

;; I don't use open-line, so overwrite its keybinding.
(define-key global-map (kbd "C-o") 'open-newline-and-indent)

;; multiple cursors
;; a good replacement for simple macros since you see the results instantly
(require 'multiple-cursors)
(require 'mc-mark-more)

(global-set-key (kbd "C-]") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "M-'") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-c M-'") 'mc/mark-all-symbols-like-this-in-defun)

(global-set-key (kbd "M-@") 'mc/mark-next-symbol-like-this)

(defun wh/copy-for-code-snippet (region-start region-end)
  "Indent the current selection by four spaces on each line, and
copy to the clipboard."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties region-start region-end)))
    (with-temp-buffer
      (insert selected-text)
      (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-region (point-min) (point-max)))))

;; Delete the whole line, without adding to the kill ring.
;; If the region is active, delete that instead.
(global-set-key (kbd "C-S-k") #'whole-line-or-region-delete)
(global-set-key (kbd "C-c k") #'whole-line-or-region-delete)

;; Copy the whole line to the kill ring.
(global-set-key (kbd "C-c C-k")
                #'whole-line-or-region-copy-region-as-kill)

(defun wh/indent-four-spaces (beg end)
  "Indent the text in the active region by four spaces.
Handy when editing markdown."
  (interactive "r")
  (indent-rigidly beg end 4))

(defun wh/unindent-four-spaces (beg end)
  "Decrease the indent of the text in the active region by four spaces.
Handy when editing markdown."
  (interactive "r")
  (indent-rigidly beg end -4))

(defun wh/shebang ()
  "Insert a shebang in the current buffer, and mark the file as executable."
  (interactive)
  ;; We need a file on the file system, or we won't be able to chmod it.
  (unless (file-exists-p (buffer-file-name))
    (basic-save-buffer))
  
  (set-file-modes (buffer-file-name)
                  (file-modes-symbolic-to-number "u+rwx" (file-modes (buffer-file-name))))

  (let ((python-interpreter "#!/usr/bin/env python")
        (bash-interpreter "#!/bin/bash")
        (racket-lang "#lang racket")
        interpreter)
    (cond
     ;; Choose shebang based on filename.
     ((f-ext-p (buffer-file-name) "py")
      (setq interpreter python-interpreter))
     ((f-ext-p (buffer-file-name) "sh")
      (setq interpreter bash-interpreter))
     ((f-ext-p (buffer-file-name) "rkt")
      (setq interpreter racket-lang))
     ;; Prompt for the shebang.
     (t
      (setq interpreter (completing-read "Interpreter: " (list bash-interpreter python-interpreter)))))
    ;; Insert shebang and ensure we're in the right major mode.
    (goto-char (point-min))
    (insert interpreter "\n\n")
    (cond
     ((equal interpreter bash-interpreter) (sh-mode))
     ((equal interpreter python-interpreter) (python-mode)))))

(defun wh/apply-on-region (beg end func)
  "Apply FUNC to the active region, replacing it with the result."
  (interactive "r\na")
  (let ((text (delete-and-extract-region beg end)))
    (insert (apply func (list text)))))

(defun wh/kill-buffer-file-name ()
  "Add the file name of the current buffer to the kill ring."
  (interactive)
  (kill-new
   ;; Handle both opened files and dired buffers.
   (or (buffer-file-name) default-directory)))

(use-package change-inner
  :bind ("M-i" . change-inner))

;; TODO: send a PR to change-inner to add this behaviour.
(defadvice change-inner (around change-inner-delete activate)
  "Don't add the removed text to the kill ring.
I'm frequently removing text between double-quotes so I can
replace it with a value from the clipboard. Thus it's annoying if
the kill-ring gets modified by `change-inner'."
  (let (kill-ring)
    ad-do-it))

(use-package hungry-delete
  :bind ("C-d" . hungry-delete-forward))

(use-package iedit
  ;; iedit sets up this key binding when you first use it, but it's nice
  ;; to have the keybinding generally available.
  :bind (("C-;" . iedit-mode)
         ("C-c i" . iedit-mode)))

;; `comment-dwim' is good, but it adds a comment to end of the line,
;; which is rarely useful.
(defun wh/comment-dwim ()
  "Toggle commenting on the current line.
If the region is active, toggle commenting on the whole region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      ;; Comment the current line, then move to the next.
      (save-excursion
        (let ((line-start (progn (beginning-of-line) (point)))
              (line-end (progn (end-of-line) (point))))
          (comment-or-uncomment-region line-start line-end)))
      (forward-line))))

(global-set-key (kbd "M-;") #'wh/comment-dwim)

(use-package string-edit-at-point
  :bind (("C-c C-c" . string-edit-at-point)))

(use-package crux
  ;; Generally, we use C-foo for line-based commands, and M-foo for
  ;; word-based commands, so kill the rest of the line with C-backspace.
  :bind (("<C-backspace>" . crux-kill-line-backwards)
         ;; Mnemonic: change name
         ("C-c C-n" . crux-rename-buffer-and-file)))

;; TODO: write this as a command in erefactor.
(defun wh/insert-provide ()
  (interactive)
  (let ((filename (f-filename (buffer-file-name))))
    (insert (format "(provide '%s)" (f-no-ext filename)))
    (insert (format "\n;;; %s ends here" filename))))

;; Disable fringe now we're using diff-hl-mode. TODO: find a better file for this.
(use-package flycheck
  :init
  (setq flycheck-indication-mode nil))

(require 'flymake)
(setq flymake-fringe-indicator-position nil)

(require 'backup-each-save)
(defun wh/show-backups ()
  "Show the directory containing all the backups of the current buffer."
  (interactive)
  (dired
   (f-dirname (backup-each-save-compute-location
               (or (buffer-file-name)
                   (f-expand "foo" default-directory))))))

;; Automatically save when tabbing out of a buffer.
;; Inspired by http://ngnghm.github.io/blog/2015/08/03/chapter-2-save-our-souls/
(use-package super-save
  :demand
  :config
  ;; Ensure that we trigger saving when switching between windows,
  ;; even when using ace-window.
  (add-to-list 'super-save-triggers #'ace-window)

  (super-save-mode +1)
  :diminish super-save-mode)

(defadvice super-save-command (around wh/dont-super-save-gpg activate)
  "It's very tedious to type the password every time I change
a symmetrically-encrypted GPG file. Require explict saving in this case."
  (unless (s-ends-with-p ".gpg" (buffer-file-name))
    ad-do-it))

;; http://pragmaticemacs.com/emacs/insert-file-name/
(defun bjm/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

(require 'bool-flip)
(global-set-key (kbd "C-c t") #'bool-flip-do-flip)

;; http://stackoverflow.com/a/4459159/509706
(defun aj/toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;; No mnemonic for this keybinding, it's simply one available in major
;; modes that I use (python binds a lot of C-c prefixes).
(global-set-key (kbd "C-c C-b") #'aj/toggle-fold)

;; Allow repeated C-SPC to pop the mark.
(setq set-mark-command-repeat-pop t)

(provide 'editing-customisations)
