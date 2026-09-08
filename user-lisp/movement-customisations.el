;;; -*- lexical-binding: nil; -*-
;; To be consistent with our paredit keybindings, use super for syntatic movement.
(global-set-key (kbd "s-a") #'beginning-of-defun)

;; An easier way to navigate to Beginning of function.
(global-set-key (kbd "C-c b") #'beginning-of-defun)

;; avy -- quickly jump to an arbitrary word or line
(require 'avy)
(setq avy-case-fold-search nil)
;; Only consider the current window (i.e. the current buffer we're
;; focused in).
(setq avy-all-windows nil)
(setq avy-keys
      (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(define-key global-map (kbd "<f11>") #'avy-goto-word-or-subword-1)
(global-set-key (kbd "<f10>") #'avy-goto-line)

;; Use `n' and `p' as movement keys in *compilation* buffers.
;; M-n and M-p are already bound, but n/p are less typing and ag.el
;; has trained me to expect those bindings.
(require 'compile)
(define-key compilation-mode-map (kbd "n") #'compilation-next-error)
(define-key compilation-mode-map (kbd "p") #'compilation-previous-error)

(defun wh/single-window-then-split ()
  "Make this window the only window, then split and focus on the right window."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1))

(global-set-key (kbd "C-c s") #'wh/single-window-then-split)

(require 'modalka-customisations)

(provide 'movement-customisations)
