;;; modalka-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "modalka" "modalka.el" (22004 6423 913102 132000))
;;; Generated autoloads from modalka.el

(autoload 'modalka-define-key "modalka" "\
Register translation from ACTUAL-KEY to TARGET-KEY.

\(fn ACTUAL-KEY TARGET-KEY)" nil nil)

(autoload 'modalka-define-kbd "modalka" "\
Register translation from ACTUAL-KBD to TARGET-KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn ACTUAL-KBD TARGET-KBD)" nil nil)

(autoload 'modalka-define-keys "modalka" "\
Register many translations described by PAIRS.

Every pair should be of this form:

  (ACTUAL-KEY TARGET-KEY)

\(fn &rest PAIRS)" nil nil)

(autoload 'modalka-define-kbds "modalka" "\
Rester many translation described by PAIRS.

Every pair should be of this form:

  (ACTUAL-KEY TARGET-KEY)

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn &rest PAIRS)" nil nil)

(autoload 'modalka-remove-key "modalka" "\
Unregister translation from KEY.

\(fn KEY)" nil nil)

(autoload 'modalka-remove-kbd "modalka" "\
Unregister translation from KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn KBD)" nil nil)

(autoload 'modalka-remove-keys "modalka" "\
Unregister translation for KEYS.

\(fn &rest KEYS)" nil nil)

(autoload 'modalka-remove-kbds "modalka" "\
Unregister translation for KBDS.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn &rest KBDS)" nil nil)

(autoload 'modalka-mode "modalka" "\
Toggle `modalka-mode' minor mode.

With a prefix argument ARG, enable `modalka-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode setups translation of key bindings according to
configuration created previously with `modalka-define-key' and
`modalka-define-keys'.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; modalka-autoloads.el ends here
