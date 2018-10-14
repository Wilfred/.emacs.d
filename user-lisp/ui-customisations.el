;; scroll one row at at time
(setq scroll-step 1)

;; Show a scrollbar when moving.
(unless (getenv "TRAVIS")
  (global-yascroll-bar-mode))

;; always highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-when-point-inside-paren t)

;; Don't show the git branch on the line.
(setq mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        mode-line-position
        mode-line-modes
        mode-line-misc-info))

;; Show debug-on-error and debug-on-quit status in the modeline.
(mode-line-debug-mode)

;; always highlight line that cursor is on, unless a mode requests
;; otherwise.
(defvar use-hl-line t
  "Whether we should use `hl-line-mode' in this buffer.
Defaults to `t'.")
(make-variable-buffer-local 'use-hl-line)

(defun wh/activate-hl-line ()
  "Enable `hl-line-mode' unless `use-hl-line` says otherwise."
  (when (and use-hl-line (not (active-minibuffer-window)))
    (hl-line-mode)))

(add-hook 'after-change-major-mode-hook #'wh/activate-hl-line)

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

;; name buffers foo<directory> foo<other_directory> rather than just numbering
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

;; Show *Help* text using `foo' style quoting rather than curly
;; quotes. I prefer the help formatting to match the underlying
;; docstring.
(setq text-quoting-style 'grave)

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
  ;; Mnemonic 'switch'.
  :bind ("C-c s" . cbm-switch-buffer))

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

(global-set-key (kbd "C-x 5") #'transpose-frame)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-char-position 'left)
  (setq aw-translate-char-function #'downcase))

(global-set-key (kbd "C-x o") #'ace-window)

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

;; https://www.emacswiki.org/emacs/TransposeWindows
 (defun transpose-windows ()
   "Transpose two windows.  If more or less than two windows are visible, error."
   (interactive)
   (unless (= 2 (count-windows))
     (user-error "There are not 2 windows."))
   (let* ((windows (window-list))
          (w1 (car windows))
          (w2 (nth 1 windows))
          (w1b (window-buffer w1))
          (w2b (window-buffer w2)))
     (set-window-buffer w1 w2b)
     (set-window-buffer w2 w1b)))

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

(use-package interaction-log
  :config
  (interaction-log-mode +1)
  (global-set-key
   (kbd "C-h C-l")
   (lambda () (interactive) (display-buffer ilog-buffer-name))))

;; Show unfinished command keystrokes more promptly than the default.
;; Taken from https://github.com/wasamasa/dotemacs/blob/master/init.org#adjust-keystroke-echo-timeout
(setq echo-keystrokes 0.5)

;; Avoid showing ?? in the mode line when we have long lines.
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#fix-line-number-mode
(setq line-number-display-limit-width 10000)

;; Use normal path formatting when disambiguating buffer names.
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#unique-buffer-names
(setq uniquify-buffer-name-style 'forward)

;; Show eldoc more promptly.
(setq eldoc-idle-delay 0.1)

;; Just kill the current buffer rather than prompting.
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;; http://emacs.stackexchange.com/a/10444/304
;; http://blog.vivekhaldar.com/post/4809065853/dotemacs-extract-interactively-change-font-size
(defun wh/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))

(defun wh/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

(defun wh/switch-major-mode-buffer (major-mode)
  (interactive
   (list
    (read
     (completing-read
      "Major mode: "
      (elisp-refs--filter-obarray #'commandp)))))
  (let* ((bufs (buffer-list))
         (matching-bufs (--filter (eq major-mode (with-current-buffer it major-mode)) bufs))
         (bufs-with-paths (--map (cons (buffer-file-name it) it) matching-bufs))
         (chosen-buf
          (cdr (assoc (completing-read "Buffer: " bufs-with-paths)
                      bufs-with-paths))))
    (switch-to-buffer chosen-buf)))

(defun wh/switch-buffers-same-mode ()
  (interactive)
  (let* ((matching-bufs (--filter (eq major-mode (with-current-buffer it major-mode))
                                  (buffer-list)))
         (bufs-with-names (--map
                           (cons
                            (let ((proj-name (with-current-buffer it (projectile-project-name))))
                              (if proj-name
                                  (format "%s (%s)" (buffer-name it) proj-name)
                                (buffer-name it)))
                            it)
                           matching-bufs))
         (chosen-buf
          (cdr (assoc (completing-read "Buffer: " bufs-with-names)
                      bufs-with-names))))
    (switch-to-buffer chosen-buf)))

(defun wh/switch-magit-status-buffer ()
  "Allow switching between open magit status buffers."
  (interactive)
  (let* ((buffers (--filter (eq #'magit-status-mode (with-current-buffer it major-mode))
                            (buffer-list)))
         (bufs-with-names (--map (cons
                                  (with-current-buffer it
                                    (format "%s %s"
                                            (s-pad-right 15 " " (projectile-project-name))
                                            (f-abbrev default-directory)))
                                  it)
                                 buffers))
         (chosen-buf
          (cdr (assoc (completing-read "Git project: " bufs-with-names)
                      bufs-with-names))))
    (switch-to-buffer chosen-buf)))

;; Since `C-x b' is for switching buffers, use `C-x m' (mnemonic:
;; Magit) to switch magit status buffers.  This clobbers the
;; keybinding for `compose-mail', which I don't use.
(global-set-key (kbd "C-x m") #'wh/switch-magit-status-buffer)

;; Loosely based on `erc-remove-text-properties-region'.
(defun wh/remove-text-properties-region ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(defun wh/prep-for-screenshot ()
  "Don't highlight the cursor or its position."
  (interactive)
  (hl-line-mode -1)
  (setq cursor-type nil)
  (show-paren-mode -1))

(defun wh/prep-for-screenshot-undo ()
  "Revert `wh/prep-for-screenshot'."
  (interactive)
  (hl-line-mode)
  (setq cursor-type 'box)
  (show-paren-mode))

;; https://emacs.stackexchange.com/a/5343/304
(defun wh/tone-down-fringes ()
  (interactive)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

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
