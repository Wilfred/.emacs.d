;;; dpaste-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dpaste" "dpaste.el" (21686 56648 180971 188000))
;;; Generated autoloads from dpaste.el

(autoload 'dpaste-region "dpaste" "\
Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option.

\(fn BEGIN END TITLE &optional ARG)" t nil)

(autoload 'dpaste-buffer "dpaste" "\
Post the current buffer to dpaste.com and yank the url to the
kill-ring.

With a prefix argument, use hold option.

\(fn TITLE &optional ARG)" t nil)

(autoload 'dpaste-region-or-buffer "dpaste" "\
Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option.

\(fn TITLE &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dpaste-autoloads.el ends here
