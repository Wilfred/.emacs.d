(require 'multiple-cursors)

(global-set-key (kbd "M-'") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-c M-'") 'mc/mark-all-symbols-like-this-in-defun)

(global-set-key (kbd "M-@") 'mc/mark-next-symbol-like-this)
