;;; swoop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (swoop-from-evil-search swoop-function swoop-from-isearch
;;;;;;  swoop-line-length-over80 swoop-migemo swoop-pcre-regexp swoop-multi
;;;;;;  swoop) "swoop" "swoop.el" (21423 59406 217176 416000))
;;; Generated autoloads from swoop.el

(autoload 'swoop "swoop" "\
Search through words within the current buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-multi "swoop" "\
Search words across currently opened multiple buffers.
Ignore non file buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-pcre-regexp "swoop" "\
Use PCRE like regexp to swoop.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-migemo "swoop" "\
Japanese words matching with the alphabet.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-line-length-over80 "swoop" "\
Get over 80 colomn number linees.

\(fn)" t nil)

(autoload 'swoop-from-isearch "swoop" "\
During isearch, switch over to swoop.

\(fn)" t nil)

(autoload 'swoop-function "swoop" "\
Show function list in buffer judging from major-mode and regexp.
Currently c-mode only.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-from-evil-search "swoop" "\
During evil-search, switch over to swoop.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("swoop-edit.el" "swoop-lib.el" "swoop-pkg.el")
;;;;;;  (21423 59406 273142 783000))

;;;***

(provide 'swoop-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swoop-autoloads.el ends here
