;;; loc-changes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "loc-changes" "loc-changes.el" (21926 5297
;;;;;;  33638 864000))
;;; Generated autoloads from loc-changes.el

(autoload 'loc-changes-goto-line "loc-changes" "\
Position `point' at LINE-NUMBER of the current buffer. If
COLUMN-NUMBER is given, position `point' at that column just
before that column number within the line. Note that the beginning of
the line starts at column 0, so the column number display will be one less
than COLUMN-NUMBER. For example COLUMN-NUMBER 1 will set before the first
column on the line and show 0.

The Emacs `goto-line' docstring says it is the wrong to use that
function in a Lisp program. So here is something that I proclaim
is okay to use in a Lisp program.

\(fn LINE-NUMBER &optional COLUMN-NUMBER)" t nil)

(autoload 'loc-changes-add-and-goto "loc-changes" "\
Add a marker at LINE-NUMBER and record LINE-NUMBER and its
marker association in `loc-changes-alist'.

\(fn LINE-NUMBER &optional OPT-BUFFER)" t nil)

(autoload 'loc-changes-clear-buffer "loc-changes" "\
Remove all location-tracking associations in BUFFER.

\(fn &optional OPT-BUFFER)" t nil)

(autoload 'loc-changes-reset-position "loc-changes" "\
Update `loc-changes-alist' so that the line number of point is
used to when aline number is requested.

Updates any existing line numbers referred to in marks at this
position.

This may be useful for example in debugging if you save the
buffer and then cause the debugger to reread/reevaluate the file
so that its positions are will be reflected.

\(fn &optional OPT-BUFFER NO-INSERT)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loc-changes-autoloads.el ends here
