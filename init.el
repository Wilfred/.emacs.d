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

; for 3rd party code that we aren't modifying, we just install as
; packages
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

; marmalade is a third party repo that anyone can submit to, so has
; many more packages
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

; always close the minibuffer on C-x o:
; <jlf> wilfredh: you could before-advise other-window to quit first
; if the minibuffer is active.. but it would be better to break that
; habit :)

(require 'ui-customisations)
(require 'startup-customisations)

(require 'file-customisations)
(require 'editing-customisations)

(if (eq system-type 'darwin)
    (require 'os-x-fixes))

(defun start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (find-file
   (format "~/scratch/%s" file-name)))

; switch on which-func-mode for all major modes that support it
; (which-func-mode shows which function or class that point is in)
(which-func-mode 1)

; Clipboard
; ---------
;
; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)
;
; show contents of kill ring on demand
(defun show-kill-ring ()
  "Show the contents of the kill ring in a pop-up"
  (interactive)
  (popup-menu 'yank-menu))
(global-set-key "\C-cy" 'show-kill-ring)

; TODO: increase kill ring size

; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode)

; reduce how often we get 'directory too big' problems:
(setq ido-max-directory-size 100000)


; when using ido for opening files, show last modified first:
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old traditional M-x
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

; TODO: make this case-sensitive
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(global-set-key (kbd "<f12>") 'isearch-forward-at-point)

; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;
;
; automatically finish quotes, brackets etc according to mode
(require 'autopair)
(autopair-global-mode)

(require 'completion-customisations)
(require 'structured-text-customisations)
(require 'html-customisations)
(require 'css-customisations)
(require 'python-customisations)
(require 'javascript-customisations)
(require 'coffeescript-customisations)
(require 'xml-customisations)
(require 'lisp-customisations)
(require 'haskell-customisations)
(require 'c-customisations)
(require 'sh-customisations)
(require 'java-customisations)

(require 'potato-customisations)

(require 'git-customisations)
(require 'search-tools)
(require 'conflicts)

(global-set-key (kbd "<f8>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f9>") 'flymake-goto-next-error)



; auto-highlight-symbol -- highlights the symbol under cursor
; elsewhere in the buffer
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(global-set-key (kbd "M-n") 'ahs-forward)
(global-set-key (kbd "M-p") 'ahs-backward)

;; crontab mode for files named
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("crontab.*?\\'" . crontab-mode))

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
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(js2-function-param-face ((((class color)) (:foreground "Green")))))

(message "Spent %.2f seconds executing .emacs.d/init.el."
         (time-difference *emacs-load-start* (current-time)))
