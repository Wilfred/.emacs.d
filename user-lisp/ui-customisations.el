; colour scheme
(add-to-list 'load-path "~/.emacs.d/user-lisp/color-theme")

(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)

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

; name buffers foo<directory> foo<other_directory> rather than just numbering
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(provide 'ui-customisations)