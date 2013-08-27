;;; highlight-symbol-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (highlight-symbol-occur highlight-symbol-query-replace
;;;;;;  highlight-symbol-prev-in-defun highlight-symbol-next-in-defun
;;;;;;  highlight-symbol-prev highlight-symbol-next highlight-symbol-list-all
;;;;;;  highlight-symbol-remove-all highlight-symbol-at-point highlight-symbol-mode)
;;;;;;  "highlight-symbol" "highlight-symbol.el" (20993 4242 316629
;;;;;;  948000))
;;; Generated autoloads from highlight-symbol.el

(autoload 'highlight-symbol-mode "highlight-symbol" "\
Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'.

\(fn &optional ARG)" t nil)

(autoload 'highlight-symbol-at-point "highlight-symbol" "\
Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.

\(fn)" t nil)

(autoload 'highlight-symbol-remove-all "highlight-symbol" "\
Remove symbol highlighting in buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-list-all "highlight-symbol" "\
List all symbols highlighted in the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-next "highlight-symbol" "\
Jump to the next location of the symbol at point within the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-prev "highlight-symbol" "\
Jump to the previous location of the symbol at point within the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-next-in-defun "highlight-symbol" "\
Jump to the next location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-prev-in-defun "highlight-symbol" "\
Jump to the previous location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-query-replace "highlight-symbol" "\
Replace the symbol at point with REPLACEMENT.

\(fn REPLACEMENT)" t nil)

(autoload 'highlight-symbol-occur "highlight-symbol" "\
Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.

\(fn &optional NLINES)" t nil)

;;;***

;;;### (autoloads nil nil ("highlight-symbol-pkg.el") (20993 4242
;;;;;;  421313 344000))

;;;***

(provide 'highlight-symbol-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-symbol-autoloads.el ends here
