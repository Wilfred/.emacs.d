; Editing customisations -- general text munging when typing.

; redraw *before* handling input events to improve syntax highlighting responsiveness
; discussed at http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/
(setq redisplay-dont-pause t)

; Automatically indent the new line when we hit enter
(define-key global-map (kbd "RET") 'newline-and-indent)


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
(global-set-key (kbd "M-z") 'zap-up-to-char)


; kill-word is less useful than kill-symbol
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

(global-set-key (kbd "M-<backspace>") 'backward-kill-symbol)

; to be consistent with C-M-f as forward-sexp, bind C-M-backspace to backward-kill-sexp
; and C-M-d to forward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)



(defun toggle-case-next-char ()
  "Toggles the case of the next character after point.
The point is also moved one character forward."
  (interactive)
  ; note next-char is a string
  (let ((next-char (buffer-substring (point) (1+ (point))))
        (case-fold-search nil)) ; case sensitive
    (if (string-match "[a-z]" next-char)
        (upcase-region (point) (1+ (point)))
      (downcase-region (point) (1+ (point)))))
  (forward-char))

; toggling on char is more useful than capitalising a whole word since
; it doesn't break camelcase
(global-set-key (kbd "M-c") 'toggle-case-next-char)


(defun transpose-symbols (arg)
  "Interchange sybmols around point, leaving point at end of them.
With prefix arg ARG, effect is to take symbol before or around point
and drag it forward past ARG other symbol (backward if ARG negative).
If ARG is zero, the symbol around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-symbol arg))

; bind it to the usual transpose-word key combination
(global-set-key (kbd "M-t") 'transpose-symbols)

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

;; bound to Ctrl with + on the keypad
(global-set-key (kbd "<C-up>") 'increment-number-decimal)
(global-set-key (kbd "<C-down>") 'decrement-number-decimal)


(provide 'editing-customisations)
