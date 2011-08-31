; colour scheme
(add-to-list 'load-path "~/.emacs.d/user-lisp/color-theme")

(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)
                                        ;
; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

; prettier mode line
; todo: show whether or not we're in recursive edit/elisp debugger
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
   " "
   mode-line-mule-info
   mode-line-client
   mode-line-remote
   mode-line-frame-identification

    ;; the buffer name with the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "("
      (propertize "%l" 'face 'font-lock-type-face) ","
      (propertize "%c" 'face 'font-lock-type-face)
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "]"


    ;; was this buffer modified since the last save?
    '(:eval (when (and (buffer-modified-p) (not buffer-read-only))
              (concat " [" (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified")
                      "]")))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat " ["  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only")
                      "]")))

    minor-mode-alist  ;; list of minor modes
    ))


(provide 'ui-customisations)