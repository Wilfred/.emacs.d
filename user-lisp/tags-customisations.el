;;; tags-customisations.el

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 1 October 2012

;; Don't prompt "Keep current list of tags tables also? (y or n)"
;; every time I switch to a new folder with its own TAGS file.
;; http://emacs.stackexchange.com/q/14802/304
(setq tags-add-tables t)

;; Don't prompt "Tags file ... has changed, read new contents?""
(setq tags-revert-without-query t)

;; Tags operations should be case sensitive. Otherwise
;; xref-find-definitions can find definitions that we don't want.
(setq tags-case-fold-search nil)

(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)

(global-set-key (kbd "C-c M-.") #'dumb-jump-go)
(global-set-key (kbd "C-c M-,") #'dumb-jump-back)

(provide 'tags-customisations)
