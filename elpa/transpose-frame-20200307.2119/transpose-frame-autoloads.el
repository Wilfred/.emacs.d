;;; transpose-frame-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "transpose-frame" "transpose-frame.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from transpose-frame.el

(autoload 'transpose-frame "transpose-frame" "\
Transpose windows arrangement at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'flip-frame "transpose-frame" "\
Flip windows arrangement vertically at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'flop-frame "transpose-frame" "\
Flop windows arrangement horizontally at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'rotate-frame "transpose-frame" "\
Rotate windows arrangement 180 degrees at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'rotate-frame-clockwise "transpose-frame" "\
Rotate windows arrangement 90 degrees clockwise at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'rotate-frame-anticlockwise "transpose-frame" "\
Rotate windows arrangement 90 degrees anti-clockwise at FRAME.
Omitting FRAME means currently selected frame.

\(fn &optional FRAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transpose-frame" '("transpose-frame-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; transpose-frame-autoloads.el ends here
