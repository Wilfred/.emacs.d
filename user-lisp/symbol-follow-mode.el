(defun symbol-follow-highlight ()
  (interactive)
  (message "hello world"))

(symbol-follow-highlight)

(define-minor-mode symbol-follow-mode
  "Toggle Symbol Follow mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
     
When symbol-follow-mode is enabled, the symbol under point is
highlighted and all instances of that symbol in the current
buffer. M-n / M-p moves between them.

Influenced by Vim's FIXME."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Symbol-Follow"
  ;; The minor mode bindings.
  '(([C-f] . symbol-follow-highlight)))

