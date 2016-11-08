;;; tags-customisations.el

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 1 October 2012

;; Don't prompt "Keep current list of tags tables also? (y or n)"
;; every time I switch to a new folder with its own TAGS file.
;; http://emacs.stackexchange.com/q/14802/304
(setq tags-add-tables t)

(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "C-c M-.") #'xref-find-definitions-other-window)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)

(provide 'tags-customisations)
