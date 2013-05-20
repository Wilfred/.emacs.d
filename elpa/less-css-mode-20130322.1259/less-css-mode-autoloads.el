;;; less-css-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flymake-less-css-init less-css-mode less-css-compile)
;;;;;;  "less-css-mode" "less-css-mode.el" (20876 65488 991344 775000))
;;; Generated autoloads from less-css-mode.el

(autoload 'less-css-compile "less-css-mode" "\
Compiles the current buffer to css using `less-css-lessc-command'.

\(fn)" t nil)

(autoload 'less-css-mode "less-css-mode" "\
Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(autoload 'flymake-less-css-init "less-css-mode" "\
Flymake support for LESS files

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("less-css-mode-pkg.el") (20876 65489 54578
;;;;;;  129000))

;;;***

(provide 'less-css-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; less-css-mode-autoloads.el ends here
