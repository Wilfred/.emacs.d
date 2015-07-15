;;; load-relative-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "load-relative" "load-relative.el" (21926 5113
;;;;;;  712576 38000))
;;; Generated autoloads from load-relative.el

(autoload '__FILE__ "load-relative" "\
Return the string name of file/buffer that is currently begin executed.

The first approach for getting this information is perhaps the
most pervasive and reliable. But it the most low-level and not
part of a public API, so it might change in future
implementations. This method uses the name that is recorded by
readevalloop of `lread.c' as the car of variable
`current-load-list'.

Failing that, we use `load-file-name' which should work in some
subset of the same places that the first method works. However
`load-file-name' will be nil for code that is eval'd. To cover
those cases, we try `buffer-file-name' which is initially
correct, for eval'd code, but will change and may be wrong if the
code sets or switches buffers after the initial execution.

As a last resort, you can pass in SYMBOL which should be some
symbol that has been previously defined if none of the above
methods work we will use the file-name value find via
`symbol-file'.

\(fn &optional SYMBOL)" nil nil)

(autoload 'find-file-noselect-relative "load-relative" "\
Read relative FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files.

\(fn FILENAME &optional NOWARN RAWFILE WILDCARDS)" nil nil)

(autoload 'with-relative-file "load-relative" "\
Read the relative FILE into a temporary buffer and evaluate BODY
in this buffer.

\(fn FILE &rest BODY)" nil t)

(put 'with-relative-file 'lisp-indent-function '1)

(autoload 'load-relative "load-relative" "\
Load an Emacs Lisp file relative to Emacs Lisp code that is in
the process of being loaded or eval'd.

FILE-OR-LIST is either a string or a list of strings containing
files that you want to loaded. If SYMBOL is given, the location of
of the file of where that was defined (as given by `symbol-file' is used
if other methods of finding __FILE__ don't work.

\(fn FILE-OR-LIST &optional SYMBOL)" nil nil)

(autoload 'require-relative "load-relative" "\
Run `require' on an Emacs Lisp file relative to the Emacs Lisp code
that is in the process of being loaded or eval'd. The symbol used in require
is the base file name (without directory or file extension) treated as a
symbol.

WARNING: it is best to to run this function before any
buffer-setting or buffer changing operations.

\(fn RELATIVE-FILE &optional OPT-FILE OPT-PREFIX)" nil nil)

(autoload 'require-relative-list "load-relative" "\
Run `require-relative' on each name in LIST which should be a list of
strings, each string being the relative name of file you want to run.

\(fn LIST &optional OPT-PREFIX)" nil t)

(autoload 'provide-me "load-relative" "\
Call `provide' with the feature's symbol name made from
source-code's file basename sans extension. For example if you
write (provide-me) inside file ~/lisp/foo.el, this is the same as
writing: (provide 'foo).

With a prefix, that prefix is prepended to the `provide' So in
the previous example, if you write (provide-me \"bar-\") this is the
same as writing (provide 'bar-foo)

\(fn &optional PREFIX)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; load-relative-autoloads.el ends here
