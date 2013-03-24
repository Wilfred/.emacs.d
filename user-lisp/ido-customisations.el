

; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode)

; reduce how often we get 'directory too big' problems:
(setq ido-max-directory-size 100000)


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

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old traditional M-x
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(provide 'ido-customisations)
