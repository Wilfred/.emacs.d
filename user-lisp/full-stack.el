(require 'dash)

(defun full-stack--indent (form highlight-index-path)
  (with-temp-buffer
    (emacs-lisp-mode)
    (cl-prettyprint form)

    ;; Find the first non-whitespace character.
    (goto-char (point-min))
    (while (looking-at (rx (or whitespace "\n")))
      (forward-char))

    ;; Step over forms according to the path.
    (--each highlight-index-path
      (forward-char 1)
      (forward-sexp it)
      (while (looking-at (rx (or whitespace "\n")))
        (forward-char)))

    ;; Highlight the sexp formed.
    (let ((region-start (point))
          (region-end (progn (forward-sexp 1) (point))))
      (text-mode)
      (add-face-text-property
       region-start region-end
       font-lock-constant-face))

    ;; `cl-prettyprint' adds leading and trailing newlines, remove
    ;; those.
    (->> (buffer-string)
         (s-chop-prefix "\n")
         (s-chop-suffix "\n"))))

(defun full-stack--subform-index-path (form subforms &optional accum)
  "Given a list of progressively more specific SUBFORMS,
return a list of indices needed to retrace those SUBFORMS in FORM.

Example:

Form: (a b (c (d e f)))
Subforms ((c (d e f)) (d e f) e)

Return: (2 1 1)"
  (catch 'found
    (when (null subforms)
      (throw 'found nil))

    (--each-indexed form
      (when (equal it (car subforms))
        (throw 'found
               (cons it-index
                     (full-stack--subform-index-path
                      it (cdr subforms))))))))

(defun full-stack--format (fn args)
  (let* ((formatted-args
          (--map
           (cond
            ((memq it '(nil t))
             (prin1-to-string it))
            ((keywordp it)
             (prin1-to-string it))
            ((symbolp it)
             (format "'%s" it))
            (t
             (prin1-to-string it)))
           args)))
    (cond
     ((byte-code-function-p fn)
      "<bytecode>")
     (args
      (format "(%s %s)"
              fn
              (s-join " " formatted-args)))
     (t
      (format "(%s)" fn)))))

(defun full-stack--buffer ()
  (let ((buf (get-buffer-create "*full-stack*")))
    (pop-to-buffer buf)))

(defun full-stack ()
  (interactive)
  (full-stack--buffer)
  (erase-buffer)
  (let (frames)
    (let* ((i 0)
           (frame (backtrace-frame i)))
      (catch 'done
        (while frame
          (push frame frames)
          (setq i (1+ i))
          (setq frame (backtrace-frame i)))))
    (let ((last-func nil)
          unevalled-frames)
      (dolist (frame frames)
        (-let [(evalled func . args) frame]
          (if evalled
              (progn
                (when unevalled-frames
                  (setq unevalled-frames (nreverse unevalled-frames))
                  (-let* (((_ . first-form) (car unevalled-frames))
                          (subforms (-map #'cdr (cdr unevalled-frames)))
                          (indices (full-stack--subform-index-path
                                    first-form subforms)))
                    (insert (full-stack--indent first-form indices)
                            "\n"))

                  (setq unevalled-frames nil))

                (insert (propertize
                         (full-stack--format func args)
                         'face 'font-lock-variable-name-face)
                        "\n"))
            (push frame unevalled-frames))

          (when (functionp func)
            (insert (format "--- %s\n" func)))))))
  (goto-char (point-min)))
