;;;; Editing customisations -- general text munging when typing.
(require 's)
(require 'dash)

;; highlight region whenever mark is active
(transient-mark-mode t)

;; overwrite when text is selected
(delete-selection-mode t)

;; Automatically indent the new line when we hit enter
(define-key global-map (kbd "RET") 'newline-and-indent)

;; zap-to-char but don't delete the character itself
;; bind it to the usual zap-to-char shortcut
(setq ajz/forward-only t)
(global-set-key (kbd "M-z") #'ace-jump-zap-up-to-char-dwim)

(defun zap-up-to-non-whitespace ()
  "Zap up to, but not including, the first non-whitespace character."
  (interactive)
  (while (looking-at (rx (or "\n" (char space))))
    (delete-char 1)))

;; Bind it to a similar keybinding to zap-up-to-char.
(global-set-key (kbd "M-Z") 'zap-up-to-non-whitespace)

;; kill-word is less useful than kill-symbol
(autoload 'forward-symbol "thingatpt")
(defun wh/kill-symbol (arg)
  "Kill characters forward until encountering the end of a symbol.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (forward-symbol arg) (point))))

(global-set-key (kbd "M-d") #'wh/kill-symbol)


(defun wh/backward-kill-symbol (arg)
  "Kill characters backward until encountering the beginning of a symbol.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-symbol (- arg)))

(global-set-key (kbd "C-<backspace>") #'wh/backward-kill-symbol)

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

(defun format-symbol (string format)
  "Convert a given string to a specified formatting convention.

 (format-symbol \"fooBar\" 'constant) => \"FOO_BAR\""
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

(defun cycle-symbol-case ()
  "Convert toggle symbol at mark between the forms \"fooBar\",
\"FooBar\", \"FOO_BAR\" and \"foo_bar\"."
  (interactive)
  (let* ((symbol (symbol-name (symbol-at-point)))
        (symbol-bounds (bounds-of-thing-at-point 'symbol))
        (bound-start (car symbol-bounds))
        (bound-end (cdr symbol-bounds)))
    (when symbol-bounds
      (goto-char bound-start)
      (kill-forward-chars (- bound-end bound-start))
      (cond
       ((re-match-p "[a-z-]+$" symbol) (insert (format-symbol symbol 'variable-underscore)))
       ((re-match-p "[a-z_]+$" symbol) (insert (format-symbol symbol 'camelcase-lower)))
       ((re-match-p "[a-z]+" symbol) (insert (format-symbol symbol 'camelcase)))
       ((re-match-p "[A-Z][a-z]+" symbol) (insert (format-symbol symbol 'constant)))
       (t (insert (format-symbol symbol 'variable-underscore)))))))

(global-set-key (kbd "C-M-c") 'cycle-symbol-case)


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
(require 'expand-region)
(require 'python-el-fgallina-expansions)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'move-text)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; ace-jump-mode -- quickly jump to an arbitrary word or line
(require 'ace-jump-mode)
(setq act-jump-case-fold nil)
(define-key global-map (kbd "<f11>") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c <f11>") 'ace-jump-line-mode)

(require 'ez-query-replace)

(define-key global-map (kbd "M-%") 'ez-query-replace)
(define-key global-map (kbd "C-c M-%") 'ez-query-replace-repeat)

;; open line and indent is a frequent and useful operation
;; so we can go from this (where `|` represents the cursor:
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

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

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
  (goto-char (point-min))

  ;; We need a file on the file system, or we won't be able to chmod it.
  (unless (file-exists-p (buffer-file-name))
    (basic-save-buffer))
  
  (set-file-modes (buffer-file-name)
                  (file-modes-symbolic-to-number "u+rwx" (file-modes (buffer-file-name))))
  (insert (ido-completing-read "Interpreter: " (list "#!/bin/bash" "#!/bin/env python")))
  (insert "\n\n"))

(defun wh/apply-on-region (beg end func)
  "Apply FUNC to the active region, replacing it with the result."
  (interactive "r\na")
  (let ((text (delete-and-extract-region beg end)))
    (insert (apply func (list text)))))

(require 'change-inner)
(global-set-key (kbd "M-i") #'change-inner)

(global-set-key (kbd "C-.") #'goto-last-change)
(global-set-key (kbd "C-,") #'goto-last-change-reverse)

(global-set-key (kbd "C-d") #'hungry-delete-forward)

(provide 'editing-customisations)
