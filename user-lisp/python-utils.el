(require 'file-utils)

(defcustom virtualenvs-root (expand-file-name "~/.envs")
  "Absolute path to the directory that contains all the virtualenvs.")

(defun virtualenv-workon ()
  "Convenience for setting `python-shell-virtualenv-path'."
  (interactive)
  (let* ((virtualenv-names
          (no-dot-directories (directory-files virtualenv-base-path)))
         (virtualenv-name (ido-completing-read "Virtualenv: " virtualenv-names))
         (virtualenv-path (file-path-join virtualenv-base-path virtualenv-name)))
    (setq python-shell-virtualenv-path virtualenv-path)))

(defun virtualenv-search ()
  "Search the soruce code in the current virtualenv for
a specific search string."
  (interactive)
  (unless python-shell-virtualenv-path
    (error "Need to set `python-shell-virtualenv-path`, see `virtualenv-workon`"))
  (let ((search-term (read-from-minibuffer "Search virtualenv for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point)))))
        (libraries-path
         (file-path-join python-shell-virtualenv-path "lib/python2.7/site-packages")))
    (ag/search search-term libraries-path)))
