(require 'highlight-symbol)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use highlight-symbol-mode in all programming modes
(add-hook 'prog-mode-hook '(lambda () (highlight-symbol-mode 1)))

;; but don't highlight anything if there's only one occurrence of this
;; symbol.
(setq highlight-symbol-highlight-single-occurrence nil)

(require 'diminish)
(diminish 'highlight-symbol-mode)

;; no delay before highlighting
(setq highlight-symbol-idle-delay 0)

;; http://www.emacswiki.org/emacs/ZapToISearch , via Steve Purcell
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char. The deleted region does not include the isearch
word. This is meant to be bound only in isearch mode. The point
of this function is that oftentimes you want to delete some
portion of text, one end of which happens to be an active isearch
word.

The observation to make is that if you use isearch a lot to
move the cursor around (as you should, it is much more efficient
than using the arrows), it happens a lot that you could just
delete the active region between the mark and the point, not
include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)))

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; TODO: this doesn't work for searching for exclamation marks. Not sure why.
(global-set-key (kbd "<f12>") #'swiper)
(global-set-key (kbd "C-c <f12>") #'swiper-all)

;; Show the index of the current match in addition to the total
;; matches with ivy (used by swiper). Anzu style.
(require 'ivy)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)

(provide 'isearch-customisations)
