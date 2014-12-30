(require 'ag)
(require 'dash)
(require 's)
(require 'f)
(require 'python)

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
  (let ((search-term (read-from-minibuffer
                      (format "Search %s virtualenv for: "
                              (f-filename python-shell-virtualenv-path))
                      (virtualenv-search--dwim-at-point)))
        (libraries-path
         (f-join python-shell-virtualenv-path "lib/python2.7/site-packages")))
    (ag/search search-term libraries-path)))

(define-key python-mode-map (kbd "C-c v s") 'virtualenv-search)

(provide 'python-utils)
