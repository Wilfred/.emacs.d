;;; modalka-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "modalka" "modalka.el" (22005 29916 529471
;;;;;;  778000))
;;; Generated autoloads from modalka.el

(defvar modalka-excluded-modes nil "\
List of major modes for which `modalka-mode' should not be activated.

This variable is considered when Modalka is enabled globally via
`modalka-global-mode'.")

(custom-autoload 'modalka-excluded-modes "modalka" t)

(autoload 'modalka-define-key "modalka" "\
Register translation from ACTUAL-KEY to TARGET-KEY.

\(fn ACTUAL-KEY TARGET-KEY)" nil nil)

(autoload 'modalka-define-kbd "modalka" "\
Register translation from ACTUAL-KBD to TARGET-KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn ACTUAL-KBD TARGET-KBD)" nil nil)

(autoload 'modalka-remove-key "modalka" "\
Unregister translation from KEY.

\(fn KEY)" nil nil)

(autoload 'modalka-remove-kbd "modalka" "\
Unregister translation from KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode').

\(fn KBD)" nil nil)

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

(defvar modalka-global-mode nil "\
Non-nil if Modalka-Global mode is enabled.
See the command `modalka-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `modalka-global-mode'.")

(custom-autoload 'modalka-global-mode "modalka" nil)

(autoload 'modalka-global-mode "modalka" "\
Toggle Modalka mode in all buffers.
With prefix ARG, enable Modalka-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Modalka mode is enabled in all buffers where
`modalka--maybe-activate' would do it.
See `modalka-mode' for more information on Modalka mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; modalka-autoloads.el ends here
