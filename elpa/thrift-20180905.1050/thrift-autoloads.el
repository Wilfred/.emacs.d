;;; thrift-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "thrift" "thrift.el" (0 0 0 0))
;;; Generated autoloads from thrift.el

(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

(autoload 'thrift-mode "thrift" "\
Major mode for editing Thrift files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "thrift" '("thrift-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; thrift-autoloads.el ends here
