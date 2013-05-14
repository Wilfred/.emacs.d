(require 'file-utils)
(require 'ag)
(require 'dash)
(require 's)

(defcustom virtualenvs-root (expand-file-name "~/.envs")
  "Absolute path to the directory that contains all the virtualenvs.")

(defun virtualenv-workon ()
  "Convenience for setting `python-shell-virtualenv-path'."
  (interactive)
  (unless (file-exists-p virtualenvs-root)
    (error "`virtualenvs-root' is set to %s, which doesn't exist" virtualenvs-root))
  (let* ((virtualenv-names
          (no-dot-directories (directory-files virtualenvs-root)))
         
         (virtualenv-name
          (ido-completing-read
           (format
            "Virtualenv (currently %s): "
            (if python-shell-virtualenv-path
              (file-name-nondirectory python-shell-virtualenv-path)
              "not set"))
           virtualenv-names))
         (virtualenv-path (file-path-join virtualenvs-root virtualenv-name)))
    (setq python-shell-virtualenv-path virtualenv-path)))

(defun virtualenv-search--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point and return a search term for its definition."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (-when-let* ((symbol (symbol-at-point))
                 (symbol-name (symbol-name symbol)))
      (let* ((case-fold-search nil)
             (is-class-name (s-matches-p "^[A-Z]" symbol-name)))
        (format "%s %s" (if is-class-name "class" "def") symbol-name)))))

(defun virtualenv-search ()
  "Search the source code in the current virtualenv for
a specific search string."
  (interactive)
  (unless python-shell-virtualenv-path
    (error "Need to set `python-shell-virtualenv-path', see `virtualenv-workon'"))
  (let ((search-term (read-from-minibuffer "Search virtualenv for: "
                                           (virtualenv-search--dwim-at-point)))
        (libraries-path
         (file-path-join python-shell-virtualenv-path "lib/python2.7/site-packages")))
    (ag/search search-term libraries-path)))

(provide 'python-utils)
