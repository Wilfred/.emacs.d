;;;; Editing customisations -- general text munging when typing.
(require 'dash)

;; highlight region whenever mark is active
(transient-mark-mode t)

;; overwrite when text is selected
(delete-selection-mode t)

;; Automatically indent the new line when we hit enter
(define-key global-map (kbd "RET") 'newline-and-indent)

;; zap-to-char but don't delete the character itself
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
;; bind it to the usual zap-to-char shortcut
(global-set-key (kbd "M-z") 'zap-up-to-char)


;; kill-word is less useful than kill-symbol
(autoload 'forward-symbol "thingatpt")
(defun kill-symbol (arg)
  "Kill characters forward until encountering the end of a symbol.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (forward-symbol arg) (point))))

(global-set-key (kbd "M-d") 'kill-symbol)


(defun backward-kill-symbol (arg)
  "Kill characters backward until encountering the beginning of a symbol.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-symbol (- arg)))

(global-set-key (kbd "C-<backspace>") 'backward-kill-symbol)

;; to be consistent with C-M-f as forward-sexp, bind C-M-backspace to backward-kill-sexp
;; and C-M-d to forward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

(defun toggle-case-next-char ()
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
(global-set-key (kbd "M-c") 'toggle-case-next-char)

(autoload 're-find-all "regexp-utils")
(autoload 're-search-p "regexp-utils")
(autoload 're-match-p "regexp-utils")

(defun string-contains-p (substring string)
  "Returns t if STRING contains SUBSTRING."
  (re-search-p (regexp-quote substring) string))

(defun format-symbol (string format)
  "Convert a given string to a specified formatting convention.

 (format-symbol \"fooBar\" 'constant) => \"FOO_BAR\""
  (let ((components))
    ;; split the string into its word components
    (if (string-contains-p "_" string)
        (setq components (split-string string "_"))
      (setq components (re-find-all "[A-Z]?[a-z]+" string)))
    ;; format each component as a lowercase string
    (setq components (mapcar 'downcase components))
    (cond
     ((eq format 'constant) (mapconcat 'upcase components "_"))
     ((eq format 'camelcase) (mapconcat 'toggle-case-first-char components ""))
     ((eq format 'camelcase-lower) (toggle-case-first-char
                                    (mapconcat 'toggle-case-first-char components "")))
     ((eq format 'variable-underscore) (mapconcat (lambda (x) x) components "_"))
     ((eq format 'variable-hyphen) (mapconcat (lambda (x) x) components "-"))
     (t (error "Unknown symbol format")))))

(defun toggle-case-first-char (string)
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


(defun transpose-symbols (arg)
  "Interchange sybmols around point, leaving point at end of them.
With prefix arg ARG, effect is to take symbol before or around point
and drag it forward past ARG other symbol (backward if ARG negative).
If ARG is zero, the symbol around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-symbol arg))

;; bind it to the usual transpose-word key combination
(global-set-key (kbd "M-t") 'transpose-symbols)

;; based on my-increment-number-decimal from http://www.emacswiki.org/emacs/IncrementNumber
;; unlike that version, we only change the number if point is on a number
(defun increment-number-decimal (&optional arg)
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

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "<C-up>") 'increment-number-decimal)
(global-set-key (kbd "<C-down>") 'decrement-number-decimal)

;; don't use shift to set the mark, or caps lock creates regions the whole time
(setq-default shift-select-mode nil)

;; use expand-region to grow the region according to the current mode's syntax
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'move-text)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; ace-jump-mode -- jump to an arbitrary word quickly
(require 'ace-jump-mode)
(setq act-jump-case-fold nil)
(define-key global-map (kbd "<f11>") 'ace-jump-mode)

(defun dwim-at-point ()
  "If there's an active selection, return that. Otherwise, get
the symbol at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (symbol-at-point)
        (symbol-name (symbol-at-point)))))

;; todo: investigate whether we're reinventing the wheel, since query-replace-history already exists
(defvar query-replace/history nil)
(defun query-replace-at-point (from-string to-string)
  "Replace occurrences of FROM-STRING with TO-STRING, defaulting
to the symbol at point."
  (interactive (list
                (read-from-minibuffer "Replace what? " (dwim-at-point))
                (read-from-minibuffer "With what? ")))

  ;; if we currently have point on a symbol we're replacing, go back
  (-when-let* ((current-symbol (symbol-at-point))
               (current-symbol-name (symbol-name current-symbol))
               (string-matches (string-equal current-symbol-name from-string)))
    (forward-symbol -1))

  (add-to-list 'query-replace/history
               (list (format "%s -> %s" from-string to-string)
                     from-string to-string))
  (perform-replace from-string to-string t nil nil))

(eval-when-compile (require 'cl)) ; first, second

(defun query-replace-repeat ()
  (interactive)
  (unless query-replace/history
    (error "You need to have done query-replace-at-point first"))
  (let* ((choices (mapcar 'first query-replace/history))
         (choice (ido-completing-read "Previous replaces: " choices))
         (from-with-to (cdr (assoc choice query-replace/history)))
         (from-string (first from-with-to))
         (to-string (second from-with-to)))
    (perform-replace from-string to-string
                   t nil nil)))

(define-key global-map (kbd "M-%") 'query-replace-at-point)
(define-key global-map (kbd "C-c M-%") 'query-replace-repeat)

;; multiple cursors
;; a good replacement for simple macros since you see the results instantly
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun copy-for-code-snippet (region-start region-end)
  "Indent the current selection by four spaces on each line, and
copy to the clipboard."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties region-start region-end)))
    (with-temp-buffer
      (insert selected-text)
      (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-region (point-min) (point-max)))))

(provide 'editing-customisations)
