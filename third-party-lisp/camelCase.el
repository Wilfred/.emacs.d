;;;  camelCase-mode.el --- minor mode for editing with camelCase words
;;   Copyright (C) 2001  C.R.Manning
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; History:

;; Author: C.R.Manning caroma@ai.mit.edu
;;         at http://www.ai.mit.edu/people/caroma/tools/
;; Created: 19 May 2001
;; last update: 20 May 2001 
;; Keywords: camelCase camel case word mode
;; Tested on: Gnu Emacs 20.7, XEmacs 21.4

;;; Installation 

;; Suggested GNU Emacs .emacs (or XEmacs .xemacs/init.el) initialization:
;;   (autoload 'camelCase-mode "camelCase-mode" nil t)
;;   (add-hook 'java-mode-hook '(lambda () (camelCase-mode 1)))
;; more global hooks:  find-file-hooks, post-command-hook

;;; Description:

;; camelCase-mode is a Gnu Emacs minor mode which modifies the Emacs
;; forward-word, backward-word, delete-word, etc. keystroke-commands
;; so they also work on words within a <i>camelCase</i> identifier.
;; As a minor mode, camelCase-mode can be used within any other
;; editing mode --- Java-mode, text mode, etc.  No functionality is
;; lost, since the forward s-expression, backward s-expression,
;; delete s-expression, etc. commands are still available to navigate
;; over entire camelCase names.
;; 
;; Word boundaries in a camelCase name are marked only by letter case.
;; For example lowerCapitalUPPERCase has four words.  A word may be
;; lowercase, Capitalized, UPPERCASE, or a sequence of digits.  Besides
;; non-letter to letter and letter to non-letter word boundaries,
;; word boundaries in the middle of a sequence of letters are located at
;; lowercaseCapital, CapitalCapital, lowercaseUPPERCASE,
;; CapitalUPPERCASE, and UPPERCASECapital boundaries.
;;
;; Rebound keys:
;;   M-f, M-right, C-right   camelCase-forward-word
;;   M-b, M-left,  C-left    camelCase-backward-word
;;   M-d,                    camelCase-forward-kill-word
;;   M-backspace,  C-delete  camelCase-backward-kill-word
;;   M-t                     camelCase-transpose-words
;;   M-c                     camelCase-capitalize-word
;;   M-u                     camelCase-upcase-word
;;   M-l                     camelCase-downcase-word
;;
;; camelCase-mode prefix arg: 0 turns off, 1 turns on, nil toggles mode.

;;; Subtleties

;; Handles uppercase acronyms within camelCase words.  For example
;;   "URLConnection" is like "URL-Connection", not "URLC-onnection"

;;; Limitations

;; Does not affect other Emacs functions which operate on words,
;;   such as dabbrev-mode
;; M-t transpose-words does not change case of words.  For example:
;;   - transposing "fooBar" produces "Barfoo", not "barFoo".
;;   - transposing "fooBAR" produces "BARfoo", not "BARFoo".
;;   - transposing "fooBar baz" at space produces "foobaz Bar" not "fooBaz bar"
;;   When this is an issue, capitalize words before transposing them,
;;   e.g., M-b M-c M-t for the first two examples, or M-c M-b M-t for
;;   the last one.

;;; Code:

;;; MODE:

(defvar camelCase-modeline-indicator " camelCase"
  "call (camelCase-install-mode) again if this is changed")
(defvar camelCase-mode nil) 
(make-variable-buffer-local 'camelCase-mode)
(put 'camelCase-mode 'permanent-local t)

(defun camelCase-mode (&optional arg)
  "Minor mode which overrides word command keys for editing camelCase words.

 Word boundaries in a camelCase name are marked only by letter case.
 For example lowerCapitalUPPERCase has four words.  A word may be
 lowercase, Capitalized, UPPERCASE, or a sequence of digits.  Besides
 non-letter to letter and letter to non-letter word boundaries,
 word boundaries in the middle of a sequence of letters are located at
 lowercaseCapital, CapitalCapital, lowercaseUPPERCASE,
 CapitalUPPERCASE, and UPPERCASECapital boundaries.

 Rebound keys:
   M-f, M-right*,  C-right      camelCase-forward-word
   M-b, M-left*,   C-left       camelCase-backward-word
   M-d, M-delete*, C-delete*    camelCase-forward-kill-word
   M-backspace,    C-backspace* camelCase-backward-kill-word
   M-t                          camelCase-transpose-words
   M-c                          camelCase-capitalize-word
   M-u                          camelCase-upcase-word
   M-l                          camelCase-downcase-word
 (* means only in Gnu Emacs, not in XEMacs; the original binding is not
  to the word command in XEmacs, so it is not overridden)

 camelCase-mode prefix ARG:  0 turns off, 1 turns on, nil toggles mode."
  (interactive "P")
  (setq camelCase-mode
	(if (null arg) (not camelCase-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defconst camelCase-keybindings-list
  (cond ((memq 'xemacs features) ;; xemacs uses different key syntax
	 '(
	   ("\M-f"              camelCase-forward-word)
	   ("\M-b"              camelCase-backward-word)
	   ("\M-d"              camelCase-forward-kill-word)
	   ("\M-DEL"            camelCase-backward-kill-word)
	   ("\M-t"              camelCase-transpose-words)
	   ("\M-c"              camelCase-capitalize-word)
	   ("\M-u"              camelCase-upcase-word)
	   ("\M-l"              camelCase-downcase-word)
	   ;((meta right)        camelCase-forward-word)
	   ;((meta left)         camelCase-backward-word)
	   ;((meta delete)       camelCase-forward-kill-word)
	   ((meta backspace)    camelCase-backward-kill-word)
	   ((control right)     camelCase-forward-word)
	   ((control left)      camelCase-backward-word)
	   ;((control delete)    camelCase-forward-kill-word)
	   ;((control backspace) camelCase-backward-kill-word)
	   ))
	(t ;; assume recent gnu emacs (e.g., 20.7)
	 '(
	   ("\M-f"         camelCase-forward-word)
	   ("\M-b"         camelCase-backward-word)
	   ("\M-d"         camelCase-forward-kill-word)
	   ("\M-DEL"       camelCase-backward-kill-word)
	   ("\M-t"         camelCase-transpose-words)
	   ("\M-c"         camelCase-capitalize-word)
	   ("\M-u"         camelCase-upcase-word)
	   ("\M-l"         camelCase-downcase-word)
	   ([\M-right]     camelCase-forward-word)
	   ([\M-left]      camelCase-backward-word)
	   ([\M-backspace] camelCase-backward-kill-word)
	   ([\M-delete]    camelCase-forward-kill-word)
	   ([\C-right]     camelCase-forward-word)
	   ([\C-left]      camelCase-backward-word)
	   ([\C-delete]    camelCase-forward-kill-word)
	   ([\C-backspace] camelCase-backward-kill-word)
	   ))
	))

(defconst camelCase-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (binding)
	      (define-key map (first binding) (second binding)))
	    camelCase-keybindings-list)
    (fset 'camelCase-mode-map map)
    map)
  "keymap for camelCase minor mode.")

;; install the minor mode
(defun camelCase-add-minor-mode (mode-toggle-variable-name
				 modeline-indicator-string mode-map)
  (let ((old-mode-entry (assq mode-toggle-variable-name minor-mode-alist)))
    (setq minor-mode-alist
	  (cons (list mode-toggle-variable-name modeline-indicator-string)
		(delq old-mode-entry minor-mode-alist))))
  (let ((old-map-entry (assq mode-toggle-variable-name
			     minor-mode-map-alist)))
    (setq minor-mode-map-alist
	  (cons (cons mode-toggle-variable-name mode-map)
		(delq old-map-entry minor-mode-map-alist)))))


(defun camelCase-install-mode () 
  ;; call function without causing byte-compiler warning if other not defined
  (let ((add-minor-mode-fn (if (and (memq 'xemacs features)
				    (fboundp 'add-minor-mode))
			       'add-minor-mode
			     'camelCase-add-minor-mode)))
    (funcall add-minor-mode-fn 
	     'camelCase-mode
	     camelCase-modeline-indicator
	     camelCase-mode-map)))

(camelCase-install-mode)

;;; COMMANDS:

(defconst camelCase-regexp "\\([A-Z]?[a-z]+\\|[A-Z]+\\|[0-9]+\\)"
  ;; capital must be before uppercase
  "regular expression that matches a camelCase word, defined as
Capitalized, lowercase, or UPPERCASE sequence of letters,
or sequence of digits.")

(defun camelCase-forward-word (count)
  "move point foward COUNT camelCase words"
  (interactive "p") 
  ;; search forward increments point until some match occurs;
  ;; extent of match is as large as possible at that point.
  ;; point is left at END of match.
  (if (< count 0)
      (camelCase-backward-word (- count))
    (let ((old-case-fold-search case-fold-search)
	  (case-fold-search nil)) ;; search case sensitively
      (unwind-protect 
	  (when (re-search-forward camelCase-regexp nil t count)
	    ;; something matched, just check for special case.
	    ;; If uppercase acronym is in camelCase word as in "URLNext",
	    ;; search will leave point after N rather than after L.
	    ;; So if match starting back one char doesn't end same place,
	    ;; then back-up one char.
	    (when (save-excursion     
		    (let ((search-end (point)))
		      (forward-char -1)
		      (and (looking-at camelCase-regexp)
			   (not (= search-end (match-end 0))))))
	      (forward-char -1))
	    (point))
	(setq case-fold-search old-case-fold-search)))))

(defun camelCase-backward-word (count) 
  "move point backward COUNT camelCase words"
  (interactive "p") 
  ;; search backward decrements point until some match occurs;
  ;; extent of match is as large as possible at that point.
  ;; So once point is found, have to keep decrementing point until we cross
  ;; into another word, which changes the match end.
  ;; for multiple words, have to do whole thing COUNT times.
  (if (< count 0)
      (camelCase-forward-word (- count))
    (let ((start-position (point))
	  (old-case-fold-search case-fold-search)
	  (case-fold-search nil)) ;; search case-sensitively
      (unwind-protect 
	  (while (< 0 count) 
	    (setq count (1- count))
	    (let ((start (point)))
	      (when (re-search-backward camelCase-regexp nil t)
		(let ((end-word (match-end 0)))
		  (forward-char -1)
		  (while (save-excursion
			   ;;like looking-at, but stop match at start
			   (let ((position (point)))
			     (re-search-forward camelCase-regexp start t)
			     (and (= position (match-beginning 0))
				  (= end-word (match-end 0)))))
		    (forward-char -1))
		  (forward-char 1)))))
	(setq case-fold-search old-case-fold-search))
      (if (= start-position (point)) nil (point)))))

(defun camelCase-forward-kill-word (count) 
  "kill text between current point and end of next camelCase word"
  (interactive "*p")
  (kill-region (point) (progn (camelCase-forward-word count) (point))))
(defun camelCase-backward-kill-word (count) 
  "kill text between current point and end of previous camelCase word"
  (interactive "*p")
  (kill-region (point) (progn (camelCase-backward-word count) (point))))
(defun camelCase-transpose-words (count)
  "transpose camelCase words around point, leaving point afterward.
With prefix arg COUNT, moves word before point past COUNT words
forward or backward.  If COUNT is 0, exchanges word around pont 
with word around mark."
  (interactive "*p")  
  (transpose-subr 'camelCase-forward-word count))
(defun camelCase-capitalize-word (count)
  "Capitalize word starting at point, leaving point after word."
  (interactive "*p")  
  (let ((start (point)))
    (camelCase-forward-word count)
    (capitalize-region start (point))))
(defun camelCase-upcase-word (count)
  "Make word starting at point UPPERCASE, leaving point after word."
  (interactive "*p")  
  (let ((start (point)))
    (camelCase-forward-word count)
    (upcase-region start (point))))
(defun camelCase-downcase-word (count)
  "Make word starting at point lowercase, leaving point after word."
  (interactive "*p")  
  (let ((start (point)))
    (camelCase-forward-word count)
    (downcase-region start (point))))


(provide 'camelCase)
