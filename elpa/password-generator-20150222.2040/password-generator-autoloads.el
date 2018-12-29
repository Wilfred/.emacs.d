;;; password-generator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "password-generator" "password-generator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from password-generator.el

(autoload 'password-generator-random "password-generator" "\
Feel free to rewrite this random. Just don't make it too slow.

\(fn MAX)" nil nil)

(autoload 'password-generator-get-random-string-char "password-generator" "\
You pass here string. You get random character from it.

\(fn STRING)" nil nil)

(autoload 'password-generator-generate-internal "password-generator" "\
Generates the password with given vocabulary and length

\(fn SYMBOLS-FOR-PASS PASS-LENGTH)" nil nil)

(autoload 'password-generator-simple "password-generator" "\
Minimal viable password for most of web systems. It is not secure but allows to register.

\(fn &optional PRE-LEN RETURN)" t nil)

(autoload 'password-generator-strong "password-generator" "\
The best password you can get. Some symbols does not included to make you free from problems which occurs when your shell tries interpolate $, \\ and others.

\(fn &optional PRE-LEN RETURN)" t nil)

(autoload 'password-generator-paranoid "password-generator" "\
Good thing to use if you really care about bruteforce. Not all applications handle special characters presented in such password properly. Be ready to escape special characters if you will pass such password via ssh command or so.

\(fn &optional PRE-LEN RETURN)" t nil)

(autoload 'password-generator-numeric "password-generator" "\
Yeah, there are sill reasons to use numeric passwords like credit card PIN-code

\(fn &optional PRE-LEN RETURN)" t nil)

(autoload 'password-generator-phonetic "password-generator" "\
It will be easy to remeber, because of fonetic, but not so secure...

\(fn &optional PRE-LEN RETURN)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; password-generator-autoloads.el ends here
