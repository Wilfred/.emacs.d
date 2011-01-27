(load "~/.emacs.d/php-mode.el" nil t t)

; ido mode -- fuzz completion
(ido-mode)

; show column numbers
(column-number-mode 1)

; don't show the end of long lines rather than wordwrap
(setq-default truncate-lines 1)

; web browser
(require 'w3m-load)

; don't show the tool bar, should use keyboard anyway
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; don't show scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; auctex stuff for latex editing
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
; completion, style file, multiline features
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

; auto-fill (break long lines) on by default
(setq-default auto-fill-function 'do-auto-fill)

; highlight matching brackets
(show-paren-mode 1)

; full screen mode, from http://news.ycombinator.com/item?id=1156946
(defun fullscreen ()
  (interactive)
  (let (f w l r cb b)
    (setq cb (current-buffer))
    (setq b (switch-to-buffer "*NOSUCHBUFFER*")) ;Temp buffer otherwise
    (setq f (make-frame '((fullscreen . fullboth)))) ;settings don't take effect
    (select-frame-set-input-focus f)
    (setq w (frame-width f))
    (setq l (/ (- w 80) 2))
    (setq r (- w (+ l 80)))
    (seq-default left-margin-width l
		  right-margin-width r)
    (delete-other-frames f)             ;Only one main window
    (kill-buffer b) ;Kill temp buffer - now original buffer will get new margins
    (switch-to-buffer cb)))

(defun fullscreen-off ()
  (interactive)
  (let (f cb b p)
    (setq cb (current-buffer))
    (setq b (switch-to-buffer "*NOSUCHBUFFER*")) ;Temp buffer
    (setq-default left-margin-width nil
		  right-margin-width nil)
    (kill-buffer b)
    (delete-other-frames (make-frame))
    (switch-to-buffer cb)))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

; don't show startup buffer
; init file can have at most one custom-set-variable and custom-set-faces
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; colour themes grooviness
(load "~/.emacs.d/color-theme-tangotango.el" nil t t)
(color-theme-tangotango)

; highlight current line
(hl-line-mode t)

; backups start with . as well as ending ~
(defun make-backup-file-name (filename)
  (expand-file-name
   (concat "." (file-name-nondirectory filename) "~")
   (file-name-directory filename)))
