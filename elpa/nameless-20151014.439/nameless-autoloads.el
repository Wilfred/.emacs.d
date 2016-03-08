;;; nameless-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "nameless" "nameless.el" (22235 2750 956899
;;;;;;  275000))
;;; Generated autoloads from nameless.el

(autoload 'nameless-mode "nameless" "\
Toggle Nameless mode on or off.
With a prefix argument ARG, enable Nameless mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{nameless-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'nameless-mode-from-hook "nameless" "\
Turn on `nameless-mode'.
Designed to be added to `emacs-lisp-mode-hook'.
Interactively, just invoke `nameless-mode' directly.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nameless-autoloads.el ends here
