;;; ansible-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ansible-doc" "ansible-doc.el" (21663 2280
;;;;;;  847496 851000))
;;; Generated autoloads from ansible-doc.el

(autoload 'ansible-doc "ansible-doc" "\
Show ansible documentation for MODULE.

\(fn MODULE)" t nil)

(autoload 'ansible-doc-mode "ansible-doc" "\
Minor mode for Ansible documentation.

When called interactively, toggle `ansible-doc-mode'.  With
prefix ARG, enable `ansible-doc-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ansible-doc-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ansible-doc-mode'.  Otherwise behave as if called interactively.

In `ansible-doc-mode' provide the following keybindings for
Ansible documentation lookup:

\\{ansible-doc-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ansible-doc-autoloads.el ends here
