;;; el2markdown-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "el2markdown" "el2markdown.el" (21967 8744
;;;;;;  917798 635000))
;;; Generated autoloads from el2markdown.el

(autoload 'el2markdown-view-buffer "el2markdown" "\
Convert comment section to markdown and display in temporary buffer.

\(fn)" t nil)

(autoload 'el2markdown-write-file "el2markdown" "\
Convert comment section to markdown and write to file.

\(fn &optional FILE-NAME OVERWRITE-WITHOUT-CONFIRM)" t nil)

(autoload 'el2markdown-write-readme "el2markdown" "\
Generate README.md, designed to be used in batch mode.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; el2markdown-autoloads.el ends here
