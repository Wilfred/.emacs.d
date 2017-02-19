;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Sean Farley <sean@farley.io>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; Stock Python debugger ipdb

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:ipdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; realgud-loc-pat that describes a ipdb location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   > /usr/bin/zonetab2pot.py(15)<module>()
;; or MS Windows:
;;   > c:\\mydirectory\\gcd.py(10)<module>
(setf (gethash "loc" realgud:ipdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^> \\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\)(\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" realgud:ipdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^ipdb[>] "
       ))

;;  realgud-loc-pat that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:ipdb-pat-hash)
      realgud-python-backtrace-loc-pat)

;;  realgud-loc-pat that describes location in a pytest error
(setf (gethash "pytest-error" realgud:ipdb-pat-hash)
      realgud-pytest-error-loc-pat)

;;  Regular expression that describes location in a flake8 message
(setf (gethash "flake8-msg" realgud:ipdb-pat-hash)
      realgud-flake8-msg-loc-pat)

;;  realgud-loc-pat that describes a "breakpoint set" line. For example:
;;     Breakpoint 1 at /usr/bin/ipdb:7
(setf (gethash "brkpt-set" realgud:ipdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) at[ \t\n]+\\(.+\\):\\([0-9]+\\)\\(\n\\|$\\)"
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a "delete breakpoint" line
;; Python 3 includes a file name and line number; Python 2 doesn't
(setf (gethash "brkpt-del" realgud:ipdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)"
       :num 1))

(setf (gethash "font-lock-keywords" realgud:ipdb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
	 (2 realgud-backtrace-number-face)
	 (4 font-lock-function-name-face nil t))     ; t means optional.

	;; Parameter sequence, E.g. gcd(a=3, b=5)
	;;                             ^^^^^^^^^
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))

	;; File name. E.g  file '/test/gcd.py'
	;;                 ------^^^^^^^^^^^^-
	("[ \t]+file '\\([^ ]+*\\)'"
	 (1 realgud-file-name-face))

	;; Line number. E.g. at line 28
        ;;                  ---------^^
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 realgud-line-number-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (ipdb-frames-match-current-line
	;;  (0 ipdb-frames-current-frame-face append))
	))

(setf (gethash "ipdb" realgud-pat-hash) realgud:ipdb-pat-hash)

(defvar realgud:ipdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'finish' and the value is
the ipdb command to use, like 'return'")

(setf (gethash "ipdb" realgud-command-hash) realgud:ipdb-command-hash)

;; Mappings between ipdb-specific names and GUD names
(setf (gethash "finish" realgud:ipdb-command-hash) "return")
(setf (gethash "kill" realgud:ipdb-command-hash) "quit")
(setf (gethash "backtrace" realgud:ipdb-command-hash) "where")
;; Clear in Python does both the usual “delete” and “clear”
(setf (gethash "delete" realgud:ipdb-command-hash) "clear %p")
(setf (gethash "clear" realgud:ipdb-command-hash) "clear %X:%l")
(setf (gethash "eval" realgud:ipdb-command-hash) "pp %s")

;; Unsupported features:
(setf (gethash "shell" realgud:ipdb-command-hash) "*not-implemented*")
(setf (gethash "frame" realgud:ipdb-command-hash) "*not-implemented*")

(provide-me "realgud:ipdb-")
