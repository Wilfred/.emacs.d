;;; langdoc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (langdoc-define-help-mode) "langdoc" "langdoc.el"
;;;;;;  (21214 22620 52711 35000))
;;; Generated autoloads from langdoc.el

(autoload 'langdoc-define-help-mode "langdoc" "\
Define help-mode and describe-symbol functions.
It defines MODE-PREFIX-mode which is a major mode to show help strings,
and defines MODE-PREFIX-describe-symbol to show help strings in
MODE-PREFIX-mode.  MODE-PREFIX-describe-symbol takes a string to show
a full documentation in a help buffer.  DESCRIPTION is a description
of MODE-PREFIX-mode.  HELPBUF-NAME is a buffer name for
MODE-PREFIX-mode.

POINTED-SYM-FN is a function which recieves no arguments and returns a
string pointed by the cursor.  MODE-PREFIX-describe-symbol uses
POINTED-SYM-FN when it is interactively called.  SYMBOLS is a list of
strings to complete the argument of MODE-PREFIX-describe-symbol.
MAKE-DOCUMENT-FN is a function which takes a string and returns the
string which is a full document of the argument.

LINK-REGEXP is a regexp to make links for MODE-PREFIX-describe-symbol.
If NIL, MODE-PREFIX-describe-symbol does not make any links in help
buffers.  LINKED-STR-FN is a function which takes substrings matched
in LINK-REGEXP and returns a string to be linked.  MAKE-LINK-FN is a
function which takes same arguments as LINKED-STR-FN and returns a
string which is a link to other document.  PREFIX-STR and SUFFIX-STR
are the prefix and suffix of the return value of LINKED-STR-FN
respectively.

For instance, let LINK-REGEXP be \"`\\\\(.+\\\\)'\", LINKED-STR-FN
be (lambda (a b) (concat \"[\" b \"]\")), MAKE-LINK-FN
be (lambda (a b) b), and PREFIX-STR and SUFFIX-STR are \"`\" and
\"'\" respectively.

In this case, a string \"`linked-str'\" becomes \"`[linked-str]'\"
with a link to \"linked-str\" in help buffer .

\(fn MODE-PREFIX DESCRIPTION HELPBUF-NAME POINTED-SYM-FN SYMBOLS MAKE-DOCUMENT-FN &optional LINK-REGEXP LINKED-STR-FN MAKE-LINK-FN PREFIX-STR SUFFIX-STR)" nil t)

;;;***

;;;### (autoloads nil nil ("langdoc-pkg.el") (21214 22620 146639
;;;;;;  312000))

;;;***

(provide 'langdoc-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; langdoc-autoloads.el ends here
