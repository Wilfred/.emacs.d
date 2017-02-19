;;; le-python.el --- lispy support for Python. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'python)
(require 'json)

(defun lispy-trim-python (str)
  "Trim extra Python indentation from STR.

STR is a string copied from Python code. It can be that each line
of STR is prefixed by e.g. 4 or 8 or 12 spaces.
Stripping them will produce code that's valid for an eval."
  (if (string-match "\\`\\( +\\)" str)
      (let* ((indent (match-string 1 str))
             (re (concat "^" indent)))
        (apply #'concat
               (split-string str re t)))
    str))

(defun lispy-eval-python-str ()
  (let (str res bnd)
    (setq str
          (save-excursion
            (cond ((region-active-p)
                   (setq str (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
                   (if (= (cl-count ?\n str) 0)
                       str
                     ;; get rid of "unexpected indent"
                     (replace-regexp-in-string
                      (concat
                       "^"
                       (save-excursion
                         (goto-char (region-beginning))
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (point))))
                      "" (lispy--string-dwim))))
                  ((looking-at lispy-outline)
                   (string-trim-right
                    (lispy--string-dwim
                     (lispy--bounds-dwim))))
                  ((setq bnd (lispy-bounds-python-block))
                   (lispy-trim-python
                    (lispy--string-dwim bnd)))
                  ((lispy-bolp)
                   (string-trim-left
                    (lispy--string-dwim
                     (lispy--bounds-c-toplevel))))
                  (t
                   (cond ((lispy-left-p))
                         ((lispy-right-p)
                          (backward-list))
                         (t
                          (error "Unexpected")))
                   (setq bnd (lispy--bounds-dwim))
                   (ignore-errors (backward-sexp))
                   (while (or (eq (char-before) ?.)
                              (eq (char-after) ?\())
                     (backward-sexp))
                   (setcar bnd (point))
                   (lispy--string-dwim bnd)))))
    (replace-regexp-in-string
     ",\n +" ","
     (replace-regexp-in-string
      "\\\\\n +" "" str))))

(defun lispy-bounds-python-block ()
  (if (save-excursion
        (when (looking-at " ")
          (forward-char))
        (python-info-beginning-of-block-p))
      (let ((indent (1+ (- (point) (line-beginning-position)))))
        (cons
         (line-beginning-position)
         (save-excursion
           (python-nav-end-of-block)
           (while (looking-at (format "[\n ]\\{%d,\\}\\(except\\|else\\)" indent))
             (goto-char (match-beginning 1))
             (python-nav-end-of-block))
           (point))))
    (cons (point)
          (save-excursion
            (end-of-line)
            (let (bnd)
              (when (setq bnd (lispy--bounds-string))
                (goto-char (cdr bnd))))
            (end-of-line)
            (while (member (char-before)
                           '(?\\ ?,))
              (end-of-line 2))
            (point)))))

(defun lispy-eval-python (&optional plain)
  (let ((res (lispy--eval-python
              (lispy-eval-python-str)
              plain)))
    (if (and res (not (equal res "")))
        (lispy-message
         (replace-regexp-in-string
          "%" "%%" res))
      (lispy-message
       (replace-regexp-in-string
        "%" "%%" lispy-eval-error)))))

(defun lispy--python-proc ()
  (let* ((proc-name "Python Internal[lispy]")
         (process (get-process proc-name)))
    (if (process-live-p process)
        process
      (let ((python-shell-font-lock-enable nil)
            (inferior-python-mode-hook nil)
            (python-binary-name (python-shell-calculate-command)))
        (save-excursion
          (goto-char (point-min))
          (when (looking-at "#!\\(.*\\)$")
            (setq python-binary-name
                  (concat
                   (match-string-no-properties 1)
                   " "
                   python-shell-interpreter-args))))
        (setq process (get-buffer-process
                       (python-shell-make-comint
                        python-binary-name proc-name nil t))))
      (setq lispy--python-middleware-loaded-p nil)
      (lispy--python-middleware-load)
      process)))

(defun lispy--eval-python (str &optional plain)
  "Eval STR as Python code."
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (unless plain
      (setq str (string-trim-left str))
      (cond ((and (string-match "\\`\\(\\(?:[., ]\\|\\sw\\|\\s_\\|[][]\\)+\\) += " str)
                  (save-match-data
                    (or single-line-p
                        (and (not (string-match-p "lp\\." str))
                             (lispy--eval-python
                              (format "x=lp.is_assignment(\"\"\"%s\"\"\")\nprint (x)" str)
                              t)))))
             (setq str (concat str (format "\nprint (repr ((%s)))" (match-string 1 str)))))
            ((and single-line-p
                  (string-match "\\`\\([A-Z_a-z,0-9 ()]+\\) in \\(.*\\)\\'" str))
             (let ((vars (match-string 1 str))
                   (val (match-string 2 str)))
               (setq str (format "%s = list (%s)[0]\nprint ((%s))" vars val vars)))))
      (when (and single-line-p (string-match "\\`return \\(.*\\)\\'" str))
        (setq str (match-string 1 str))))
    (let ((res
           (cond ((or single-line-p
                      (string-match "\n .*\\'" str)
                      (string-match "\"\"\"" str))
                  (python-shell-send-string-no-output
                   str (lispy--python-proc)))
                 ((string-match "\\`\\([\0-\377[:nonascii:]]*\\)\n\\([^\n]*\\)\\'" str)
                  (let* ((p1 (match-string 1 str))
                         (p2 (match-string 2 str))
                         (p1-output (python-shell-send-string-no-output
                                     p1 (lispy--python-proc)))
                         p2-output)
                    (cond ((null p1-output)
                           (lispy-message lispy-eval-error))
                          ((null (setq p2-output (lispy--eval-python p2)))
                           (lispy-message lispy-eval-error))
                          (t
                           (concat
                            (if (string= p1-output "")
                                ""
                              (concat p1-output "\n"))
                            p2-output)))))
                 (t
                  (error "unexpected")))))
      (cond ((string-match "^Traceback.*:" res)
             (set-text-properties
              (match-beginning 0)
              (match-end 0)
              '(face error)
              res)
             (setq lispy-eval-error res)
             nil)
            ((equal res "")
             (setq lispy-eval-error "(ok)")
             "")
            (t
             (replace-regexp-in-string "\\\\n" "\n" res))))))

(defun lispy--python-array-to-elisp (array-str)
  "Transform a Python string ARRAY-STR to an Elisp string array."
  (when (and (stringp array-str)
             (not (string= array-str "")))
    (let ((parts (with-temp-buffer
                   (python-mode)
                   (insert (substring array-str 1 -1))
                   (goto-char (point-min))
                   (let (beg res)
                     (while (< (point) (point-max))
                       (setq beg (point))
                       (forward-sexp)
                       (push (buffer-substring-no-properties beg (point)) res)
                       (skip-chars-forward ", "))
                     (nreverse res)))))
      (mapcar (lambda (s)
                (if (string-match "\\`\"" s)
                    (read s)
                  (if (string-match "\\`'\\(.*\\)'\\'" s)
                      (match-string 1 s)
                    s
                    )))
              parts))))

(defun lispy-dir-string< (a b)
  (if (string-match "/$" a)
      (if (string-match "/$" b)
          (string< a b)
        t)
    (if (string-match "/$" b)
        nil
      (string< a b))))

(defun lispy-python-completion-at-point ()
  (cond ((looking-back "^\\(import\\|from\\) .*" (line-beginning-position))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (point)))
                (str
                 (format
                  "import jedi; script=jedi.Script(\"%s\",1,%d); [_x_.name for _x_ in script.completions()]"
                  line (length line)))
                (cands
                 (lispy--python-array-to-elisp
                  (lispy--eval-python str)))
                (bnd (bounds-of-thing-at-point 'symbol))
                (beg (if bnd (car bnd) (point)))
                (end (if bnd (cdr bnd) (point))))
           (list beg end cands)))
        ((lispy--in-string-p)
         (let* ((bnd-1 (lispy--bounds-string))
                (bnd-2 (or (bounds-of-thing-at-point 'symbol)
                           (cons (point) (point))))
                (str (buffer-substring-no-properties
                      (1+ (car bnd-1))
                      (1- (cdr bnd-1)))))
           (list (car bnd-2)
                 (cdr bnd-2)
                 (cl-sort (delete "./" (all-completions str #'read-file-name-internal))
                          #'lispy-dir-string<))))
        (t
         (let ((comp (python-shell-completion-at-point (lispy--python-proc))))
           (list (nth 0 comp)
                 (nth 1 comp)
                 (mapcar (lambda (s)
                           (if (string-match "(\\'" s)
                               (substring s 0 (match-beginning 0))
                             s))
                         (all-completions
                          (buffer-substring-no-properties
                           (nth 0 comp)
                           (nth 1 comp))
                          (nth 2 comp))))))))

(defvar lispy--python-arg-key-re "\\`\\(\\(?:\\sw\\|\\s_\\)+\\) ?= ?\\(.*\\)\\'"
  "Constant regexp for matching function keyword spec.")

(defun lispy--python-args (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (forward-sexp)
        (while (and (< (point) end)
                    (not (looking-at ",")))
          (forward-sexp))
        (push (buffer-substring-no-properties
               beg (point))
              res)
        (skip-chars-forward ", \n")
        (setq beg (point))))
    (nreverse res)))

(defun lispy--python-debug-step-in ()
  (re-search-forward "(" (line-end-position))
  (backward-char)
  (let* ((p-ar-beg (point))
         (p-ar-end (save-excursion
                     (forward-list)
                     (point)))
         (p-fn-end (progn
                     (skip-chars-backward " ")
                     (point)))
         (p-fn-beg (progn
                     (backward-sexp)
                     (point)))
         (fn (buffer-substring-no-properties
              p-fn-beg p-fn-end))
         (args
          (lispy--python-args (1+ p-ar-beg) (1- p-ar-end)))
         (args-key (cl-remove-if-not
                    (lambda (s)
                      (string-match lispy--python-arg-key-re s))
                    args))
         (args-normal (cl-set-difference args args-key))
         (fn-data
          (json-read-from-string
           (substring
            (lispy--eval-python
             (format "import inspect, json; json.dumps (inspect.getargspec (%s))"
                     fn))
            1 -1)))
         (fn-args
          (mapcar #'identity (elt fn-data 0)))
         (fn-defaults
          (mapcar
           (lambda (x)
             (cond ((null x)
                    "None")
                   ((eq x t)
                    "True")
                   (t
                    (prin1-to-string x))))
           (elt fn-data 3)))
         (fn-alist
          (cl-mapcar #'cons
                     fn-args
                     (append (make-list (- (length fn-args)
                                           (length fn-defaults))
                                        nil)
                             fn-defaults)))
         (fn-alist-x fn-alist)
         dbg-cmd)
    (dolist (arg args-normal)
      (setcdr (pop fn-alist-x) arg))
    (dolist (arg args-key)
      (if (string-match lispy--python-arg-key-re arg)
          (let ((arg-name (match-string 1 arg))
                (arg-val (match-string 2 arg))
                arg-cell)
            (if (setq arg-cell (assoc arg-name fn-alist))
                (setcdr arg-cell arg-val)
              (error "\"%s\" is not in %s" arg-name fn-alist)))
        (error "\"%s\" does not match the regex spec" arg)))
    (when (memq nil (mapcar #'cdr fn-alist))
      (error "Not all args were provided: %s" fn-alist))
    (setq dbg-cmd
          (mapconcat (lambda (x)
                       (format "%s = %s" (car x) (cdr x)))
                     fn-alist
                     "; "))
    (if (lispy--eval-python dbg-cmd t)
        (lispy-goto-symbol fn)
      (goto-char p-ar-beg)
      (message lispy-eval-error))))

(defun lispy-goto-symbol-python (symbol)
  (save-restriction
    (widen)
    (let ((res (ignore-errors
                 (or
                  (deferred:sync!
                      (jedi:goto-definition))
                  t))))
      (if (member res '(nil "Definition not found."))
          (let* ((symbol (python-info-current-symbol))
                 (file (car
                        (lispy--python-array-to-elisp
                         (lispy--eval-python
                          (format
                           "import inspect\ninspect.getsourcefile(%s)" symbol))))))
            (if file
                (progn
                  (find-file file)
                  (goto-char (point-min))
                  (re-search-forward
                   (concat "^def.*" (car (last (split-string symbol "\\." t)))))
                  (beginning-of-line))
              (error "Both jedi and inspect failed")))
        (unless (looking-back "def " (line-beginning-position))
          (jedi:goto-definition))))))

(defun lispy--python-docstring (symbol)
  "Look up the docstring for SYMBOL.

First, try to see if SYMBOL.__doc__ returns a string in the
current REPL session (dynamic).

Otherwise, fall back to Jedi (static)."
  (let ((dynamic-result (lispy--eval-python (concat symbol ".__doc__"))))
    (if (> (length dynamic-result) 0)
        (mapconcat #'string-trim-left
                   (split-string (substring dynamic-result 1 -1) "\\\\n")
                   "\n")
      (require 'jedi)
      (plist-get (car (deferred:sync!
                          (jedi:call-deferred 'get_definition)))
                 :doc))))

(defvar lispy--python-middleware-loaded-p nil
  "Nil if the Python middleware in \"lispy-python.py\" wasn't loaded yet.")

(defun lispy--python-middleware-load ()
  "Load the custom Python code in \"lispy-python.py\"."
  (unless lispy--python-middleware-loaded-p
    (lispy--eval-python
     (format "import imp;lp=imp.load_source('lispy-python','%s')"
             (expand-file-name "lispy-python.py" lispy-site-directory)))
    (setq lispy--python-middleware-loaded-p t)))

(defun lispy--python-arglist (symbol filename line column)
  (lispy--python-middleware-load)
  (format "%s (%s)"
          symbol
          (mapconcat #'identity
                     (delete "self"
                             (lispy--python-array-to-elisp
                              (lispy--eval-python
                               (format "lp.arglist(%s, '%s', %s, %s)"
                                       symbol filename line column))))
                     ", ")))

(provide 'le-python)

;;; le-python.el ends here
