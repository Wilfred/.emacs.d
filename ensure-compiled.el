(require 'dash)
(require 'f)

;; When a package is installed, it's byte-compiled. However, git is set
;; up to ignore .elc files (see the .gitignore file). The system that
;; installs the file therefore has .elc files, but other systems need to
;; byte-compile those directories.

;; To make matters worse, we can't just compile on startup any package
;; files that aren't compiled already, since some files fail compilation
;; every time. Instead, we compile directories that don't contain any
;; .elc files.

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun wh/ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (interactive)
  (--each (f-directories package-user-dir)
    (unless (was-compiled-p it)
      (byte-recompile-directory it 0))))

;; todo: clean up orphaned .elc files
