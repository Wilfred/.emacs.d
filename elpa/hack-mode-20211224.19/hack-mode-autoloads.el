;;; hack-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hack-mode" "hack-mode.el" (0 0 0 0))
;;; Generated autoloads from hack-mode.el

(add-to-list 'auto-mode-alist '("\\.php$" . hack-mode) t)

(add-to-list 'auto-mode-alist '("\\.hhi$" . hack-mode))

(add-to-list 'auto-mode-alist '("\\.hack$" . hack-mode))

(add-to-list 'auto-mode-alist '("\\.hck$" . hack-mode))

(add-to-list 'interpreter-mode-alist (cons (purecopy "hhvm") 'hack-mode))

(autoload 'hack-mode "hack-mode" "\
Major mode for editing Hack code.

\\{hack-mode-map\\}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hack-mode" '("hack-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hack-mode-autoloads.el ends here
