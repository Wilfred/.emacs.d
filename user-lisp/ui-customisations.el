; scroll one row at at time
(setq scroll-step 1)

;; Show a scrollbar when moving.
(global-yascroll-bar-mode)

;; always highlight matching parentheses
(show-paren-mode 1)

;; always highlight line that cursor is on, unless a mode has set d
(defvar use-hl-line t
  "Whether we should use `hl-line-mode' in this buffer.
Defaults to `t'.")
(make-variable-buffer-local 'use-hl-line)

(defun activate-hl-line ()
  "Enable `hl-line-mode' unless `use-hl-line` says otherwise."
  (when (and use-hl-line (not (active-minibuffer-window)))
    (hl-line-mode)))

(add-hook 'after-change-major-mode-hook 'activate-hl-line)

;; don't use hl-line in the minibuffer, it's not useful
;; FIXME: doesn't work
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq use-hl-line nil)))

(require 'dash)
(require 's)

(defun x-led-mask ()
  "Get the current status of the LED mask from X."
  (with-temp-buffer
    (call-process "xset" nil t nil "q")
    (let ((led-mask-string
           (->> (buffer-string)
                s-lines
                (--first (s-contains? "LED mask" it))
                s-split-words
                -last-item)))
      (string-to-number led-mask-string 16))))

(defun caps-lock-on (led-mask)
  "Return non-nil if caps lock is on."
  (eq (logand led-mask 1) 1))

(define-minor-mode caps-lock-show-mode
  "Display whether caps lock is on."
  :global t
  :lighter (:eval (if (caps-lock-on (x-led-mask)) " CAPS-LOCK" "")))

;; always truncate lines (i.e. don't wrap lines)
(setq-default truncate-lines t)

; show file name in window title
(setq frame-title-format "%b - emacs")

; name buffers foo<directory> foo<other_directory> rather than just numbering
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Highlight TODO, FIXME and BUG.
(use-package fic-mode
  :config
  (add-hook 'prog-mode-hook #'fic-mode)

  ;; TODO: work out why this highlighting doesn't work when the current
  ;; sexp/line is highlighted.
  (custom-set-faces
   '(font-lock-fic-face ((t (:foreground "Red"))) t))

  :diminish "")

;; winner-mode allows changes to frame layouts (e.g. splits) to be
;; undone with `winner-undo'.
(winner-mode 1)

;; Don't open a debugger when we quit (especially common in the
;; minibuffer). TODO: find what's triggering this.
(add-to-list 'debug-ignored-errors 'quit)

;;; ibuffer

;; use ibuffer to group buffers
(use-package ibuffer
  :config
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (size-h 10 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  :bind ("C-x C-b" . ibuffer))

;; Cycle between buffers with the same major mode.
(use-package cbm
  ;; TODO: diff-hl is clobbering this, fix.
  :bind ("C-x v" . cbm-cycle))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.0f KiB" (/ (buffer-size) 1024.0)))
   ((> (buffer-size) 1000000) (format "%7.0f MiB" (/ (buffer-size) (* 1024.0 1024.0))))
   (t (format "%9d B" (buffer-size)))))

;; automatically abbreviate long paths in grep or ack output
(defvar abbreviate-paths-when-searching nil)
(when abbreviate-paths-when-searching
  (require 'scf-mode)
  (add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
  (add-hook 'ack-and-a-half-mode-hook (lambda () (scf-mode 1))))

(defun wh/toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-x 5") 'wh/toggle-frame-split)

;; It's common for me to split the window vertically, with the same
;; buffer on both sides. This lets me easily view two different parts
;; of the same buffer.
;;
;; If this command leads to surprising behaviour, `winner-undo' will
;; restore the previous window arrangement.
(defun wh/split-window-or-repeat ()
  "If the frame isn't split vertically, split it.
If it is split, repeat the current buffer in a vertical split."
  (interactive)
  (if (= (length (window-list)) 1)
      ;; Split the window as normal.
      (progn
        (split-window-right)
        ;; When I split the window I always switch to the right
        ;; window. Switch automatically to save a few keystrokes.
        (other-window 1))
    (progn
      ;; Use a vertical split where both sides are the current buffer.
      (delete-other-windows)
      (split-window-right))))

(global-set-key (kbd "C-x 3") #'wh/split-window-or-repeat)

;; windmove allows S-<right> and S-<right> to switch between windows
;; instead of `C-x o'
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Don't let org override these keybindings.
(use-package org
  :config
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil))

(use-package paredit
  :diminish "PE")

(use-package projectile
  :diminish "")

;;; Mode-line.

;; I want a minimalistic mode line. It should only have:
;;
;; * Modified state
;; * Buffer name
;; * Top/Bot/%ge
;; * Line and column number (clearly labelled which)
;; * Major mode
;; * Minor modes that mutate the buffer (i.e. those where it would be
;;   surprising if they were on and you didn't know)
;;
;; It should also use colour rather than punctuation characters
;; wherever possible. E.g. distinguishing major from minor mode (as
;; the major mode name can, but should not, include spaces).
;;
;; I've played with `smart-mode-line` but I found the UI hard to
;; grok. It does have some great ideas that I should investigate.
;;
;; Powerline doesn't offer significantly more features than the
;; default Emacs modeline, but it's prettier. It also provides a cute
;; scrollbar on the far right.

;; Seems to prevent showing recursive editing/debugging, and the
;; colours on the inactive window in the frame are indistguishable
;; from my background color. The little scrollbar is cute though.

(provide 'ui-customisations)
