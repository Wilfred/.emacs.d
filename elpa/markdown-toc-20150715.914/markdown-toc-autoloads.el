;;; markdown-toc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "markdown-toc" "markdown-toc.el" (21967 8697
;;;;;;  679567 977000))
;;; Generated autoloads from markdown-toc.el

(autoload 'markdown-toc-version "markdown-toc" "\
Markdown-toc version.

\(fn)" t nil)

(autoload 'markdown-toc-generate-toc "markdown-toc" "\
Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC.

\(fn &optional REPLACE-TOC-P)" t nil)

;;;***

;;;### (autoloads nil nil ("markdown-toc-pkg.el") (21967 8697 803909
;;;;;;  811000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; markdown-toc-autoloads.el ends here
