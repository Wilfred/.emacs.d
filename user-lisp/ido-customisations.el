(require 'ido)

;; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; use ido-mode for more things, such as C-h f
(ido-ubiquitous-mode)

; reduce how often we get 'directory too big' problems:
(setq ido-max-directory-size 100000)

;; Show killed buffers at end when using ido for switching buffers.
(setq ido-use-virtual-buffers 'auto)

;; when using ido for opening files, show last modified first:
;; this version from http://jqian.googlecode.com/svn-history/r145/trunk/emacsconf/config/30-elisp.el
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)           ; avoid tramp
                (cond ((and (string-match "[:\\*]$" a) (not (string-match "[:\\*]$" b)))
                       nil)
                      ((and (string-match "[:\\*]$" b) (not (string-match "[:\\*]$" a)))
                         t)
                      ((and (string-match "[:\\*]$" a) (string-match "[:\\*]$" b))
                       nil)
                      (t
                         (let ((ta (nth 5 (file-attributes
                                           (concat ido-current-directory a))))
                               (tb (nth 5 (file-attributes
                                           (concat ido-current-directory b)))))
                           (cond ((and (null ta) tb) nil) ; avoid temporary buffers
                                 ((and ta (null tb)) t)
                                 ((and (null ta) (null tb)) nil)
                                 (t (if (= (nth 0 ta) (nth 0 tb))
                                        (> (nth 1 ta) (nth 1 tb))
                                      (> (nth 0 ta) (nth 0 tb)))))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

;; When opening files, create their parent directories if they don't exist.
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(provide 'ido-customisations)
