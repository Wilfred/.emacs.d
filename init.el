(setq *emacs-load-start* (current-time))


(add-to-list 'load-path "~/.emacs.d/third-party-lisp/")
(add-to-list 'load-path "~/.emacs.d/user-lisp/")

; some plugins (at least w3) install themselves here:
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(defun time-as-unixtime (most-sig-bits least-sig-bits microseconds)
  "Return the number of seconds since 1st of January 1970."
  (+ (lsh most-sig-bits 16) least-sig-bits
     (/ microseconds 1000000.0)))

(defun time-difference (from-time to-time)
  "Calculate the time difference in seconds between FROM-TIME and
TO-TIME. Parameters are assumed to be in the format returned
by (current-time)."
  (let* ((start-time (apply 'time-as-unixtime from-time))
      (finish-time (apply 'time-as-unixtime to-time)))
    (- finish-time start-time)))

(defmacro timed-require (feature)
  (let ((timing-var (make-symbol "start-time")))
    `(let ((,timing-var (current-time)))
       (require ,feature)
       (message "(require \'%s) took %.2f seconds."
                (symbol-name ,feature)
                (time-difference ,timing-var (current-time))))))

; marmalade is a third party repo that anyone can submit to, so has
; many more packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; load the packages we've installed. Note that package-install
;; byte-compiles the packages, but .elc is ignored by git so we force recompilation here
(byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
(package-initialize)

;; set exec-path according to the system's PATH
(exec-path-from-shell-initialize)

; always close the minibuffer on C-x o:
; <jlf> wilfredh: you could before-advise other-window to quit first
; if the minibuffer is active.. but it would be better to break that
; habit :)

(require 'ui-customisations)
(require 'startup-customisations)

(require 'file-customisations)
(require 'editing-customisations)
(require 'kill-ring-customisations)

(if (eq system-type 'darwin)
    (require 'os-x-fixes))

(defun start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (find-file
   (format "~/scratch/%s" file-name)))

; TODO: increase kill ring size

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


;; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;; treat space charcters as matching space characters, not like PCRE's '\s+'
(setq search-whitespace-regexp nil)

(require 'completion-customisations)
(require 'structured-text-customisations)
(require 'isearch-customisations)
(require 'html-customisations)
(require 'css-customisations)
(require 'python-customisations)
(require 'javascript-customisations)
(require 'xml-customisations)
(require 'lisp-customisations)
(require 'haskell-customisations)
(require 'c-customisations)
(require 'sh-customisations)
(require 'coffee-customisations)

(require 'potato-customisations)

(require 'git-customisations)
(require 'search-tools)

(require 'ag)
(setq ag-highlight-search 't)
(global-set-key (kbd "<f5>") 'ag-project-at-point)

(require 'conflicts)

(require 'tags-utils)

(global-set-key (kbd "<f8>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f9>") 'flymake-goto-next-error)

(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)

;; crontab mode for files named
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("crontab.*?\\'" . crontab-mode))

(require 'wiki)
(setq default-major-mode 'text-mode)
(setq wiki-directories (list (expand-file-name "~/Dropbox/Wiki/")))

(setq ring-bell-function 'ignore)

; something in the above has switched on debugging. Switch it off.
; TODO: find offending code
(setq debug-on-error nil)

; suspend seems to crash on Gnome 3, and I don't use it anyway, so just disable it
(setq cannot-suspend t)
(defun suspend-emacs (&rest)
  (interactive))
(defun suspend-frame (&rest)
  (interactive))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-plugin-defalt-face ((t nil)))
 '(ethan-wspace-face ((t (:background "#2e3434"))))
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(smerge-refined-change ((t (:background "black")))))

(message "Spent %.2f seconds executing .emacs.d/init.el."
         (time-difference *emacs-load-start* (current-time)))
