; colour scheme
(add-to-list 'load-path "~/.emacs.d/third-party-lisp/color-theme")

(when window-system
  (require 'color-theme)
  (require 'color-theme-tango)
  (color-theme-tango))

; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

; scroll one row at at time
(setq scroll-step 1)

; show x-position (ie column number) for point in buffer
(column-number-mode 1)

; always highlight matching parentheses
(show-paren-mode 1)

; always highlight line that cursor is on
(global-hl-line-mode 1)

; always truncate lines (i.e. don't wrap lines)
(setq-default truncate-lines t)

; show file name in window title
(setq frame-title-format "%b - emacs")

; name buffers foo<directory> foo<other_directory> rather than just numbering
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; highlight TODO, FIXME and BUG
(font-lock-add-keywords nil
                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))

; style diffs so that additions, removals etc are different colours
(require 'diff-mode-)

; use ibuffer to group buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

; automatically abbreviate long grep output
(require 'scf-mode)
(add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
(add-hook 'ack-and-a-half-mode-hook (lambda () (scf-mode 1)))

(defun toggle-frame-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-x 5") 'toggle-frame-split)

(provide 'ui-customisations)
