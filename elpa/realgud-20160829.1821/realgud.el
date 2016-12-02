;;; realgud.el --- A modular front-end for interacting with external debuggers

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.4.3
;; Package-Type: multi
;; Package-Requires: ((load-relative "1.2") (loc-changes "1.2") (test-simple  "1.2.0") (cl-lib "0.5") (emacs "24"))
;; URL: http://github.com/realgud/realgud/

;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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

;;; Commentary:

;; A modular, extensible GNU Emacs front-end for interacting with
;; external debuggers.
;;
;; Quick start: https://github.com/realgud/realgud/
;;
;; See URL `https://github.com/realgud/realgud/wiki/Features' for features, and
;; URL `https://github.com/realgud/realgud/wiki/Debuggers-Supported' for
;; debuggers we can handle.
;;
;; Once upon a time in an Emacs far far away and a programming-style
;; deservedly banished, there was a monolithic Cathederal-like
;; debugger front-end called gub.  This interfaced with a number of
;; debuggers, many now dead.  Is there anyone still alive that
;; remembers sdb from UNIX/32V circa 1980?
;;
;; This isn't that.  Here we make use of more modern programming
;; practices, more numerous and smaller files, unit tests, and better
;; use of Emacs primitives, e.g. buffer marks, buffer-local variables,
;; structures, rings, hash tables.  Although there is still much to be
;; desired, this code is more scalable and suitable as a common base for
;; an Emacs front-end to modern debuggers.
;;
;; Oh, and because global variables are largely banned, we can support
;; several simultaneous debug sessions.

;; If you don't see your favorite debugger, see URL
;; `https://github.com/realgud/realgud/wiki/How-to-add-a-new-debugger/'
;; for how you can add your own.

;; The debugger is run out of a comint process buffer, or you can use
;; a `realgud-track-mode' inside an existing comint shell, shell, or
;; eshell buffer.

;; To install you will need a couple of other Emacs packages
;; installed.  If you install via melpa (`package-install') or
;; `el-get', these will be pulled in automatically.  See the
;; installation instructions URL
;; `https://github.com/realgud/realgud/wiki/How-to-Install' for all
;; the ways to to install and more details on installation.

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)

(defgroup realgud nil
  "The Grand Cathedral Debugger rewrite"
  :group 'processes
  :group 'tools
  :version "24.3")

;; FIXME: extend require-relative for "autoload".
(defun realgud:load-features()
  (require-relative-list
   '(
     "./realgud/common/track-mode"
     "./realgud/common/utils"
     "./realgud/debugger/bashdb/bashdb"
     "./realgud/debugger/gdb/gdb"
     "./realgud/debugger/gub/gub"
     "./realgud/debugger/ipdb/ipdb"
     "./realgud/debugger/jdb/jdb"
     "./realgud/debugger/kshdb/kshdb"
     "./realgud/debugger/nodejs/nodejs"
     "./realgud/debugger/pdb/pdb"
     "./realgud/debugger/perldb/perldb"
     "./realgud/debugger/rdebug/rdebug"
     "./realgud/debugger/remake/remake"
     "./realgud/debugger/trepan/trepan"
     "./realgud/debugger/trepanjs/trepanjs"
     "./realgud/debugger/trepan.pl/trepanpl"
     "./realgud/debugger/trepan2/trepan2"
     "./realgud/debugger/trepan3k/trepan3k"
     "./realgud/debugger/zshdb/zshdb"
     ) "realgud-")
  )

(load-relative "./realgud/common/custom")

(defun realgud-feature-starts-with(feature prefix)
  "realgud-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1))
  (string-prefix-p (symbol-name feature) prefix)
  )

(defun realgud:loaded-features()
  "Return a list of loaded debugger features. These are the
features that start with 'realgud-' and also include standalone debugger features
like 'pydbgr'."
  (let ((result nil)
	(feature-str))
    (dolist (feature features result)
      (setq feature-str (symbol-name feature))
      (cond ((eq 't
		 (string-prefix-p feature-str "realgud-"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 (string-prefix-p feature-str "nodejs"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepan".
		 (string-prefix-p feature-str "trepan"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepanx".
		 (string-prefix-p feature-str "trepanx"))
	     (setq result (cons feature-str result)))
	    ('t nil))
	)
      )
)

(defun realgud:unload-features()
  "Remove all features loaded from this package. Used in
`realgud:reload-features'. See that."
  (interactive "")
  (let ((result (realgud:loaded-features)))
    (dolist (feature result result)
      (unless (symbolp feature) (setq feature (make-symbol feature)))
      (if (featurep feature)
	(unload-feature feature) 't))
  ))

(defun realgud:reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (realgud:unload-features)
  (realgud:load-features)
  )

;; Load everything.
(realgud:load-features)


;;; Autoloads-related code

;; This section is needed because package.el doesn't recurse into subdirectories
;; when looking for autoload-able forms.  As a workaround, we statically
;; generate our own autoloads, and force Emacs to read them by adding an extra
;; autoloded form.

;;;###autoload
(defconst realgud--recursive-autoloads-file-name "realgud-recursive-autoloads.el"
  "Where to store autoloads for subdirectory contents.")

;;;###autoload
(defconst realgud--recursive-autoloads-base-directory
  (file-name-directory
   (if load-in-progress load-file-name
     buffer-file-name)))

;;;###autoload
(with-demoted-errors "Error in RealGUD's autoloads: %s"
  (load (expand-file-name realgud--recursive-autoloads-file-name
                          realgud--recursive-autoloads-base-directory)
        t t))

(defun realgud--rebuild-recursive-autoloads ()
  "Update RealGUD's recursive autoloads.
This is needed because the package.el infrastructure doesn't
process autoloads in subdirectories; instead we create an
additional autoloads file of our own, and we load it from an
autoloaded form.  Maintainers should run this after adding
autoloaded functions, and commit the resulting changes."
  (interactive)
  (let ((generated-autoload-file
         (expand-file-name realgud--recursive-autoloads-file-name
                           realgud--recursive-autoloads-base-directory)))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (dolist (name (with-no-warnings
                    (directory-files-recursively
                     realgud--recursive-autoloads-base-directory "" t)))
      (when (file-directory-p name)
        (update-directory-autoloads name)))))

(provide-me)

;;; realgud.el ends here
