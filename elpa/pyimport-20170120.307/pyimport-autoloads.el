;;; pyimport-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pyimport" "pyimport.el" (22697 45483 528095
;;;;;;  943000))
;;; Generated autoloads from pyimport.el

(autoload 'pyimport-insert-missing "pyimport" "\
Try to insert an import for the symbol at point.
If called with a prefix, choose which import to use.

This is a simple heuristic: we just look for imports in all open Python buffers.

\(fn PREFIX)" t nil)

(autoload 'pyimport-remove-unused "pyimport" "\
Remove unused imports in the current Python buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pyimport-autoloads.el ends here
