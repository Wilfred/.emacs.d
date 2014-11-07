; scroll one row at at time
(setq scroll-step 1)

; always highlight matching parentheses
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

; always truncate lines (i.e. don't wrap lines)
(setq-default truncate-lines t)

; show file name in window title
(setq frame-title-format "%b - emacs")

; name buffers foo<directory> foo<other_directory> rather than just numbering
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; highlight TODO, FIXME and BUG
(font-lock-add-keywords nil
                        '(("\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))

;;; ibuffer

;; use ibuffer to group buffers
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.0f KiB" (/ (buffer-size) 1024.0)))
   ((> (buffer-size) 1000000) (format "%7.0f MiB" (/ (buffer-size) (* 1024.0 1024.0))))
   (t (format "%9d B" (buffer-size)))))

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


;; automatically abbreviate long paths in grep or ack output
(defvar abbreviate-paths-when-searching nil)
(when abbreviate-paths-when-searching
  (require 'scf-mode)
  (add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
  (add-hook 'ack-and-a-half-mode-hook (lambda () (scf-mode 1))))

(defun toggle-frame-split ()
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

(global-set-key (kbd "C-x 5") 'toggle-frame-split)

(defun split-this-frame ()
  "Change this frame to be split vertically, with the current buffer in both.
All other buffers are hidden.

Yes this is an obscure function, but it's pretty common in my workflow. I use
it to open related buffers easily and discard other buffers (commonly magit)."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-c C-x 2") 'split-this-frame)

(require 'paredit)
(diminish 'paredit-mode "PE")
(require 'projectile)
(diminish 'projectile-mode "Proj")

(provide 'ui-customisations)
