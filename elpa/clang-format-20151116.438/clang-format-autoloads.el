;;; clang-format-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "clang-format" "clang-format.el" (22243 58717
;;;;;;  782063 407000))
;;; Generated autoloads from clang-format.el

(autoload 'clang-format-region "clang-format" "\
Use clang-format to format the code between START and END according to STYLE.
If called interactively uses the region or the current statement if there
is no active region.  If no style is given uses `clang-format-style'.

\(fn CHAR-START CHAR-END &optional STYLE)" t nil)

(autoload 'clang-format-buffer "clang-format" "\
Use clang-format to format the current buffer according to STYLE.

\(fn &optional STYLE)" t nil)

(defalias 'clang-format 'clang-format-region)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clang-format-autoloads.el ends here
