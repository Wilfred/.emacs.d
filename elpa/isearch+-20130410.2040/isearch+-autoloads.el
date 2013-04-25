;;; isearch+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (isearch-mode-help isearch-toggle-case-fold isearchp-set-region-around-search-target
;;;;;;  isearchp-toggle-set-region isearchp-toggle-regexp-quote-yank
;;;;;;  isearchp-toggle-invisible isearchp-cycle-mismatch-removal
;;;;;;  isearchp-set-region-flag isearchp-regexp-quote-yank-flag
;;;;;;  isearchp-mouse-2-flag isearchp-drop-mismatch) "isearch+"
;;;;;;  "isearch+.el" (20857 34653))
;;; Generated autoloads from isearch+.el

(defvar isearchp-drop-mismatch nil "\
*Non-nil means remove or replace a search-string mismatch.
There are three possible values:

`replace-last' - Replace the last mismatch in the search string with
                 the latest input (e.g., replace the last typed char
                 or last yanked text).
nil            - Never remove mismatched text from the search string.
anything else  - Always remove mismatched text from the search string.

* Vanilla Isearch has the behavior of a nil value.

* Non-nil, non-`replace-last' means the search string never contains
  mismatched characters.

* `replace-last' means you see only the latest mismatched input, and
  it is available for editing, using \\<isearch-mode-map>`\\[isearch-edit-string]'.

You can cycle among the three possible values using `\\[isearchp-cycle-mismatch-removal]'.")

(custom-autoload 'isearchp-drop-mismatch "isearch+" t)

(defvar isearchp-mouse-2-flag t "\
*Non-nil means clicking `mouse-2' during Isearch yanks the selection.
In that case, you can select text with the mouse, then hit `C-s' to
search for it.

If the value is nil, yank only if the `mouse-2' click is in the echo
area.  If not in the echo area, invoke whatever `mouse-2' is bound to
outside of Isearch.")

(custom-autoload 'isearchp-mouse-2-flag "isearch+" t)

(defvar isearchp-regexp-quote-yank-flag t "\
*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this with `isearchp-toggle-regexp-quote-yank', bound to
`C-`' during isearch.")

(custom-autoload 'isearchp-regexp-quote-yank-flag "isearch+" t)

(defvar isearchp-set-region-flag nil "\
*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch.")

(custom-autoload 'isearchp-set-region-flag "isearch+" t)

(autoload 'isearchp-cycle-mismatch-removal "isearch+" "\
Cycle option `isearchp-drop-mismatch'.

\(fn)" t nil)

(autoload 'isearchp-toggle-invisible "isearch+" "\
Toggle `search-invisible'.

\(fn)" t nil)

(autoload 'isearchp-toggle-regexp-quote-yank "isearch+" "\
Toggle `isearchp-regexp-quote-yank-flag'.

\(fn)" t nil)

(autoload 'isearchp-toggle-set-region "isearch+" "\
Toggle `isearchp-set-region-flag'.

\(fn)" t nil)

(autoload 'isearchp-set-region-around-search-target "isearch+" "\
Set the region around the last search or query-replace target.

\(fn)" t nil)

(autoload 'isearch-toggle-case-fold "isearch+" "\
Toggle case folding in searching on or off.
The minor-mode lighter is `ISEARCH' for case-insensitive, `Isearch'
for case-sensitive.

\(fn)" t nil)

(autoload 'isearch-mode-help "isearch+" "\
Display information on interactive search in buffer *Help*.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("isearch+-pkg.el") (20857 34653 621882))

;;;***

(provide 'isearch+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; isearch+-autoloads.el ends here
