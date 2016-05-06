;;; jump-char-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "jump-char" "jump-char.el" (22317 2303 608297
;;;;;;  945000))
;;; Generated autoloads from jump-char.el

(autoload 'jump-char-forward "jump-char" "\
Prompt for a character, and jump to the next occurrence of that character.
Invokes `ace-jump-line-mode' when called with prefix.

When jump-char is active:

| key     | does                                                                           |
|---------+--------------------------------------------------------------------------------|
| <char>  | move to the next match in the current direction.                               |
| ;       | next match forward (towards end of buffer) see `jump-char-forward-key'         |
| ,       | next match backward (towards beginning of buffer) see `jump-char-backward-key' |
| C-c C-c | invoke `ace-jump-mode' if available                                            |

Any other key stops jump-char and edits as normal.

\(fn ARG)" t nil)

(autoload 'jump-char-backward "jump-char" "\
backward movement version of `jump-char-forward'

\(fn ARG)" t nil)

(autoload 'jump-char-forward-set-mark "jump-char" "\
See `jump-char-forward', set-mark if not active.

\(fn ARG)" t nil)

(autoload 'jump-char-backward-set-mark "jump-char" "\
See `jump-char-backward', set-mark if not active.

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jump-char-autoloads.el ends here
